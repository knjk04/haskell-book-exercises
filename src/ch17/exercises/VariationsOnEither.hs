module VariationsOnEither where

import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) e = eq e

-- From https://github.com/CarlosMChica/HaskellBook/blob/master/chapter17/ValidationApplicative.hs
instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [Failure <$> arbitrary, Success <$> arbitrary]

-- From https://github.com/CarlosMChica/HaskellBook/blob/master/chapter17/ValidationApplicative.hs
main :: IO ()
main = do
  let trigger :: Validation (String, String, String) (String, String, String)
      trigger = undefined
  quickBatch (applicative trigger)
