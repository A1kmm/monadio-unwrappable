module Control.Monad.IO.MonadIOException where

import Control.Monad.IO.Unwrappable
import Control.Monad.IO.Class
import Control.Exception

bracketIO :: MonadIOUnwrappable m d e => IO a -> (a -> IO b) -> (a -> m c) -> m c
bracketIO init cleanup action = do
  s <- unwrapState
  r <- liftIO $ bracket init cleanup (\x -> unwrapMonadIO s (action x))
  rewrapMonadIO s r
