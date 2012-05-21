module Control.Monad.IO.MonadIOException where

import Control.Monad.IO.Unwrappable
import Control.Monad.IO.Class
import Control.Exception

-- | Guarantees that an IO operation will be performed before an after executing
-- | a MonadIOUnwrappable monad. The operation will be performed even if the
-- | MonadIO contains error monads that fails, or if an exception is raised.
bracketIO :: MonadIOUnwrappable m => IO a           -- ^ The operation to perform initially.
                                     -> (a -> IO b) -- ^ The cleanup that should always be performed 
                                     -> (a -> m c)  -- ^ The monad transformer stack to execute.
                                     -> m c
bracketIO init cleanup action = do
  s <- unwrapState
  r <- liftIO $ bracket init cleanup (\x -> unwrapMonadIO s (action x))
  rewrapMonadIO s r
