module ChapterExercises where

import Data.Semigroup  
import Test.QuickCheck hiding (Failure, Success)

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

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

-- 4)

data Three a b c = Three a b c

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

-- 5)

data Four a b c d = Four a b c d

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

-- 6)

newtype BoolConj = BoolConj Bool deriving Show

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  (BoolConj _) <> (BoolConj False)   = BoolConj False
  (BoolConj False) <> (BoolConj _)   = BoolConj False

-- 7)

newtype BoolDisj = BoolDisj Bool deriving Show

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  (BoolDisj _) <> (BoolDisj _)         = BoolDisj True

-- 8)

data Or a b =
    Fst a
  | Snd b
  deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  Snd b <> _      = Snd b
  _ <> Snd b      = Snd b
  _ <> Fst a      = Fst a

-- 9) taken from https://github.com/mvaldesdeleon/haskell-book/blob/master/ch15/exercises/src/Exercises.hs

newtype Combine a b =
  Combine { unCombine :: (a -> b) } 

instance Show (Combine a b) where
  show (Combine _) = "Combine"

instance Semigroup b => Semigroup (Combine a b) where  
  (Combine f) <> (Combine g) = Combine (\a -> f a <> g a)

-- 10)

newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
  (Comp a) <> (Comp a') = Comp (a <> a')

-- 11)

data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Success b <> _          = Success b
  _ <> Success b          = Success b
  Failure a <> Failure a' = Failure (a <> a')

main3 :: IO ()
main3 = do
  let failure :: String -> Validation String Int
      failure = Failure
      success :: Int -> Validation String Int
      success = Success
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2
  

----- *** Monoid exercises *** -----

-- 1)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

main4 :: IO ()
main4 = do
  let sa  = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: TrivAssoc)
  quickCheck (mli :: Trivial -> Bool)
  quickCheck (mlr :: Trivial -> Bool)

-- 2)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

-- 3)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

-- 4)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

-- 5)  

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

-- 6) from https://github.com/mvaldesdeleon/haskell-book/blob/master/ch15/exercises/src/Exercises.hs

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine (const mempty)
  mappend = (<>)

-- 7)

instance Monoid a => Monoid (Comp a) where
  mempty = mempty
  mappend = (<>)

-- 8)

newtype Mem s a =
  Mem {
    runMem :: s -> (a, s)
  }

instance Semigroup a => Semigroup (Mem s a) where
  (<>) = undefined

instance Monoid a => Monoid (Mem s a) where
  mempty = undefined
