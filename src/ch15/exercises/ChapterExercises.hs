module ChapterExercises where

import Data.Semigroup  
import Test.QuickCheck

----- *** Semigroup exercises: implement the Semigroup instance *** -----

-- 1)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = ((a <> b) <> c) == (a <> (b <> c))

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

main :: IO ()
main = quickCheck (semigroupAssoc :: TrivAssoc)

-- 2)

newtype Identity a = Identity a deriving Eq

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

-- instance Arbitrary (Identity a) where
--   arbitrary = return 

semigroupIdentityAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupIdentityAssoc a b c = ((a <> b) <> c) == (a <> (b <> c))

type IdentityAssoc a = (Identity a) -> (Identity a) -> (Identity a) -> Bool

main2 :: IO ()
main2 = quickCheck (semigroupIdentityAssoc :: (IdentityAssoc a))
