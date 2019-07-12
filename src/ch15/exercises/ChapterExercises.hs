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

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

semigroupIdentityAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupIdentityAssoc a b c = ((a <> b) <> c) == (a <> (b <> c))

type IdentityAssoc = (Identity String) -> (Identity String) -> (Identity String) -> Bool

-- type checks but never terminates
main2 :: IO ()
main2 = quickCheck (semigroupIdentityAssoc :: (IdentityAssoc))

-- 3)

data Two a b = Two a b

instance Semigroup (Two a b) (Two c d) where
  (Two a b) <> (Two c d) = Two $ (a <> b) <> (c <> d) 
