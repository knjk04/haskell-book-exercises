module EitherMonad where

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second a) = Second (f a)

instance Applicative (Sum a) where
  pure a = Second a
  (First a) <*> _   = First a
  _ <*> (First a)   = First a
  (Second f) <*> (Second b) = Second $ f b

instance Monad (Sum a) where
  return = pure
  -- (>>=) :: m a -> (a -> m b) -> m b
  (>>=) (First x) f = First x
  (>>=) (Second x) f = f x
  
