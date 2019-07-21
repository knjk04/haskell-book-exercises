module VariationsOnEither where

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success $ f a

-- different to Either
-- (<*>) :: f (a -> b) -> f a -> f b
instance Monoid e => Applicative (Validation e) where
  pure a = Success a
  Failure f <*> _ = Failure f
  _ <*> Failure x = Failure x
  (Success f) <*> (Success x) = Success $ f x
