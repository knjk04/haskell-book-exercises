module MonadComposition where

import Control.Monad (join)

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
-- mcomp f g a = join $ f <$> (g a)
mcomp f g a = g a >>= f
