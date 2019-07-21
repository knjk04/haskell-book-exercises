module BindExercise where

import Control.Monad

-- this is (>>=) flipped
bind :: Monad m => (a -> m b) -> m a -> m b
bind f a = join $ f <$> a 
