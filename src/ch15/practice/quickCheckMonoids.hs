module QuickCheckMonoids where

import Data.Monoid
import Test.QuickCheck

asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
asc (<>) a b c =
  a <> (b <> c) == (a <> b) <> c

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)
