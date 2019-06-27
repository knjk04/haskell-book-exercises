module UsingQC where

import Test.QuickCheck

-- 1) test using QuickCheck  
half x = x / 2

-- "this property should hold"
halfIdentity = (*2) . half

identityGen :: (Arbitrary a, Num a) => Gen a
identityGen = do
  a <- arbitrary
  return a

-- instance halfGen :: (Arbitrary a, Num a) => Arbitrary a where
--   arbitrary = identityGen

-- instance (Arbitrary a, Num a) => Arbitrary a where
--   arbitrary = identityGen

testHalfIdentity :: IO ()
testHalfIdentity = quickCheck identityGen
