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

instance Monad (Reader r) where
  return = pure

  -- (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= aRb =
    -- Reader $ \r -> (aRb (ra r) r)
    -- Reader $ \r -> (aRb (ra r))
    -- Reader $ \r -> (Reader (flip aRb)) <*> (Reader ra)

    -- Reader $ \r -> ((flip aRb) r (ra r)) 

    -- (\r -> aRb (ra r))

    -- Reader $ r -> ra (r) (ra r) -- too funky

    Reader $ \r -> undefined
