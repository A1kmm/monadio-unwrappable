{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
-- | Code in this module is not directly related to CellML, but it is rather convenience code used by the solver.
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
class MonadIO m => MonadIOUnwrappable m c s | m -> c, m -> s where
  -- | Sets up state (e.g. an IORef) to be used to simulate the monad from the
  -- | IO monad.
  unwrapState :: m s
  
  -- | Maps the monad to only use IO level constructs and the state set up 
  -- | using unwrapState.
  unwrapMonadIO :: s -> m a -> IO (c a)
  
  -- | Reverses a previous unwrapMonadIO operation.
  rewrapMonadIO :: s -> c a -> m a

instance MonadIOUnwrappable IO Single () where
  unwrapState = return ()
  unwrapMonadIO _ asIO = liftM Single asIO
  rewrapMonadIO _ (Single x) = return x

newtype EitherChain a b c = EitherChain (a (Either b c))
instance (Error e, MonadIO m, MonadIOUnwrappable m c s) => MonadIOUnwrappable (ErrorT e m) (EitherChain c e) s where
  unwrapState = lift (unwrapState)
  unwrapMonadIO s m = liftM EitherChain $ unwrapMonadIO s (runErrorT m)
  rewrapMonadIO s (EitherChain v) = ErrorT (rewrapMonadIO s v)

instance (MonadIO m, MonadIOUnwrappable m c s) => MonadIOUnwrappable (ReaderT r m) c (r, s) where
  unwrapState = liftM2 (,) ask (lift unwrapState)
  unwrapMonadIO (r, s) m = unwrapMonadIO s (runReaderT m r)
  rewrapMonadIO (_, s) v = ReaderT (\_ -> rewrapMonadIO s v)

instance (MonadIO m, MonadIOUnwrappable m c s) => MonadIOUnwrappable (StateT r m) c (IORef r, s) where
  unwrapState = liftM2 (,) (get >>= (liftIO . newIORef)) (lift unwrapState)
  unwrapMonadIO (r, s) m = unwrapMonadIO s $ do
    s0 <- liftIO (readIORef r)
    (a, v') <- runStateT m s0
    liftIO (writeIORef r v')
    return a
  rewrapMonadIO (r, s) a = StateT (\_ -> liftM2 (,) (rewrapMonadIO s a) (liftIO . readIORef $ r))
