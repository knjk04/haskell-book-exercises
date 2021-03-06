module MaybeAnotherMonoid where

import Data.Monoid
import OptionalMonoid
import Test.QuickCheck
-- import Control.Monad

-- data Optional a =
--     Nada
--   | Only a

newtype First' a =
  First' { getFirst :: Optional a }
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    x <- arbitrary
    frequency [ (1, return $ First' Nada)
              , (9, return $ First' (Only x))]

-- instance Monoid a => Semigroup (First' a) where
  -- (<>) = mappend
instance Semigroup (First' a) where
  (<>) = undefined

instance Monoid a => Monoid (First' a) where
  mempty  = First' Nada
  -- MaybeAnotherMonoid.getFirst mempty  = Nada
  -- mempty Nada = Nada
  -- mappend = (<>)
  -- mappend = First' $ (<>)

  mappend (First' (Only x)) (First' Nada)     = First' (Only x)
  mappend (First' Nada) (First' (Only y))     = First' (Only y)
  mappend (First' Nada) (First' Nada)         = First' Nada
  mappend (First' (Only x)) (First' (Only y)) = First' $ Only (x <> y)
  -- mappend (First' ( Only x)) (First' ( Only y)) = First' (x <> y)

-- firstMappend :: First' a -> First' a -> First' a
firstMappend :: Monoid a => First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

-- testing left and right associativity
-- monoidLeftIdentity :: (Eq m, Monoid m, Show m) => m -> Bool
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- compiles but errors
main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend) 
  quickCheck (monoidLeftIdentity :: FstId) 
  quickCheck (monoidRightIdentity :: FstId) 
