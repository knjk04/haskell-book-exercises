module UsingQC where

import Test.QuickCheck

-- 1) test using QuickCheck  
half x = x / 2

-- "this property should hold"
-- halfIdentity :: Double -> Double
halfIdentity = (*2) . half

prop_identity :: Double -> Bool
prop_identity n = n == halfIdentity n

test :: IO ()
test = quickCheck prop_identity


