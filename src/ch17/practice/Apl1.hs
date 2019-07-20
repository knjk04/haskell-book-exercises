module Apl1 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- this isn't going to work properly
-- complains of duplicate instance declaration
instance Monoid a
  => Monoid (ZipList a) where
  mempty = ZipList []
  mappend = liftA2 mappend

-- complains of duplicate instance declaration
instance Arbitrary a
  => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary

-- complains of duplicate instance declaration
instance Arbitrary a
  => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary

instance Eq a
  => EqProp (ZipList a) where
  (=-=) = eq

instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = liftA2 mappend
