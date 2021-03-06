module ShortExercise where

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a)  = First a
  fmap f (Second a) = Second (f a)
