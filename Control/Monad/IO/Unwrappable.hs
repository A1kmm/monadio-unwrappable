{-# LANGUAGE TypeFamilies #-}
-- | Contains a class and instance for MonadIO implementations that can be run directly in MonadIO and
-- | then reconstructed to the original type, without changing the overall semantics.
module Control.Monad.IO.Unwrappable (MonadIOUnwrappable, unwrapState, unwrapMonadIO, rewrapMonadIO)
where       

import Control.Monad.Trans.Class
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.State
import Control.Monad.State.Class
import Data.IORef

data Single a = Single a

-- | Represents a MonadIO where any change further up the monad stack can be
-- | represented lower down in the stack.
class MonadIO m => MonadIOUnwrappable m where
  type MonadIOWrapType m :: * -> *
  type MonadIOStateType m :: *
  -- | Sets up state (e.g. an IORef) to be used to simulate the monad from the
  -- | IO monad.
  unwrapState :: m (MonadIOStateType m)
  
  -- | Maps the monad to only use IO level constructs and the state set up 
  -- | using unwrapState.
  unwrapMonadIO :: MonadIOStateType m -> m a -> IO (MonadIOWrapType m a)
  
  -- | Reverses a previous unwrapMonadIO operation.
  rewrapMonadIO :: MonadIOStateType m -> MonadIOWrapType m a -> m a

instance MonadIOUnwrappable IO where
  type MonadIOWrapType IO = Single
  type MonadIOStateType IO = ()
  unwrapState = return ()
  unwrapMonadIO _ asIO = liftM Single asIO
  rewrapMonadIO _ (Single x) = return x

newtype EitherChain a b c = EitherChain (a (Either b c))
instance (Error e, MonadIO m, MonadIOUnwrappable m) => MonadIOUnwrappable (ErrorT e m) where
  type MonadIOWrapType (ErrorT e m) = EitherChain (MonadIOWrapType m) e
  type MonadIOStateType (ErrorT e m) = MonadIOStateType m
  unwrapState = lift (unwrapState)
  unwrapMonadIO s m = liftM EitherChain $ unwrapMonadIO s (runErrorT m)
  rewrapMonadIO s (EitherChain v) = ErrorT (rewrapMonadIO s v)

instance (MonadIO m, MonadIOUnwrappable m) => MonadIOUnwrappable (ReaderT r m) where
  type MonadIOWrapType (ReaderT r m) = MonadIOWrapType m
  type MonadIOStateType (ReaderT r m) = (r, MonadIOStateType m)
  unwrapState = liftM2 (,) ask (lift unwrapState)
  unwrapMonadIO (r, s) m = unwrapMonadIO s (runReaderT m r)
  rewrapMonadIO (_, s) v = ReaderT (\_ -> rewrapMonadIO s v)

instance (MonadIO m, MonadIOUnwrappable m) => MonadIOUnwrappable (StateT r m) where
  type MonadIOWrapType (StateT r m) = MonadIOWrapType m
  type MonadIOStateType (StateT r m) = (IORef r, MonadIOStateType m)
  unwrapState = liftM2 (,) (get >>= (liftIO . newIORef)) (lift unwrapState)
  unwrapMonadIO (r, s) m = unwrapMonadIO s $ do
    s0 <- liftIO (readIORef r)
    (a, v') <- runStateT m s0
    liftIO (writeIORef r v')
    return a
  rewrapMonadIO (r, s) a = StateT (\_ -> liftM2 (,) (rewrapMonadIO s a) (liftIO . readIORef $ r))
