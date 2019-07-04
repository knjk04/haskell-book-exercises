module MaybeAnotherMonoid where

import Data.Monoid
import OptionalMonoid
import Test.QuickCheck

newtype First' a =
  First' { getFirst :: Optional a }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) = undefined

instance Monoid (First' a) where
  mempty = undefined

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

-- testing left and right associativity
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend) 
  quickCheck (monoidLeftIdentity :: FstId) 
  quickCheck (monoidRightIdentity :: FstId) 
