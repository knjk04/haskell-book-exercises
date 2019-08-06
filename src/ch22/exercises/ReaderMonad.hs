{-# LANGUAGE InstanceSigs #-}
module ReaderMonad where

newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader r) = Reader $ fmap f r

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader (\_ -> a) 

  -- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> (rab r (ra r))
myLiftA2 :: Applicative f =>
            (a -> b -> c)
         -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

-- taken from https://github.com/Cake42/haskell-programming-from-first-principles/blob/master/22/reader-monad.hs
instance Monad (Reader r) where
  return = pure
  -- (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> (runReader $ aRb $ ra r) r
