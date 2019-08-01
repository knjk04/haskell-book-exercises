module ReadingComprehension where

newtype Reader r a =
  Reader { runReader :: r -> a }

myLiftA2 :: Applicative f =>
            (a -> b -> c)
         -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

asks :: (r -> a) -> Reader r a
asks f = Reader f
