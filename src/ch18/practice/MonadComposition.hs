module MonadComposition where

import Control.Monad (join)

mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = join $ f <$> (g a)
