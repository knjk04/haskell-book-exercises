module UsingQC where

import Test.QuickCheck
import Data.List (sort)
import Data.Char

-- 1) test using QuickCheck  
half :: Fractional a => a -> a
half x = x / 2

-- "this property should hold"
halfIdentity :: Double -> Double
halfIdentity = (*2) . half

prop_identity :: Double -> Bool
prop_identity n = n == halfIdentity n

testIdentity :: IO ()
testIdentity = quickCheck prop_identity

-- 2) "for any list that sort is applied to,
-- this property should hold"
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status -- list is unordered
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

prop_listOrdered :: Ord a => [a] -> Bool
prop_listOrdered xs = listOrdered $ sort xs

testListOrdered :: IO ()
testListOrdered = quickCheck (prop_listOrdered :: [Char] -> Bool)

-- 3) test associative and commutative properties of addition
prop_plusAssociative :: Integral a => a -> a -> a -> Bool
prop_plusAssociative x y z = x + (y + z) == (x + y) + z

testPlusAssociative :: IO ()
testPlusAssociative = quickCheck (prop_plusAssociative :: Int -> Int -> Int -> Bool)

prop_plusCommutative :: Integral a => a -> a -> Bool
prop_plusCommutative x y = x + y == y + x

testPlusCommutative :: IO ()
testPlusCommutative = quickCheck (prop_plusCommutative :: Int -> Int -> Bool)

-- 4) same as above but for multiplication

prop_timesAssociative :: Integral a => a -> a -> a -> Bool
prop_timesAssociative x y z = x * (y * z) == (x * y) * z

testTimesAssociative :: IO ()
testTimesAssociative = quickCheck (prop_timesAssociative :: Int -> Int -> Int -> Bool)

prop_timesCommutative :: Integral a => a -> a -> Bool
prop_timesCommutative x y = x * y == y * x

testTimesCommutative :: IO ()
testTimesCommutative = quickCheck (prop_timesCommutative :: Int -> Int -> Bool)

-- 5) prove the relationship between quot and rem, and div and mod

prop_quotRem :: (Eq a, Integral a) => NonZero a -> NonZero a -> Bool
prop_quotRem (NonZero x) (NonZero y) = (quot x y) * y + (rem x y) == x

testQuotRem :: IO ()
testQuotRem = quickCheck (prop_quotRem :: NonZero Int -> NonZero Int -> Bool)

-- prop_divMod :: Integral a => a -> a -> Bool
prop_divMod :: (Eq a, Integral a) => NonZero a -> NonZero a -> Bool
prop_divMod (NonZero x) (NonZero y) = (div x y) * y + (mod x y) == x  

testDivMod :: IO ()
testDivMod = quickCheck (prop_divMod :: NonZero Int -> NonZero Int -> Bool)

-- 6) Check whether (^) is associative and/or commutative using QuickCheck

-- genNonZero :: Gen Int 
-- genNonZero = choose (1, 10000)

-- -- hypothesis: not associative
-- prop_ExponentNotAssociative :: Integral a => NonZero a -> NonZero a -> Bool
-- prop_ExponentNotAssociative (NonZero x) (NonZero y) | x == y    = False -- it is associative when x == y
--                                                     | otherwise = (x ^ y) /= (y ^ y)

-- test_ExponentNotAssociative :: IO ()
-- test_ExponentNotAssociative = quickCheck (prop_ExponentNotAssociative :: NonZero Int -> NonZero Int -> Bool)

-- 7) test that reversing a list twice is the same as the list not reversed
prop_reverseList :: Eq a => [a] -> Bool
prop_reverseList xs = ((reverse . reverse) xs) == id xs

testReverseList :: IO ()
testReverseList = quickCheck (prop_reverseList :: [Char] -> Bool)

-- 8) test ($)

-- prop_testParens1 :: Eq a => (a -> Bool) -> a -> Bool
-- prop_testParens1 f a = (f $ a) == (f a)

-- test_parens1 :: IO ()
-- test_parens1 = quickCheck (prop_testParens1 :: (Int -> Bool) -> Int -> Bool)

-- prop_testParens2 f g = f . g == \x -> f (g x)

-- test_parens2 :: IO ()
-- test_parens2 = quickCheck prop_testParens2
  


-- 9) check if the functions are equal

-- It's fine that this test fails 
prop_consAppend :: Eq a => [a] -> [a] -> Bool
prop_consAppend xs ys = (foldr (:) xs ys) == ((++) xs ys)

testConsAppend :: IO ()
testConsAppend = quickCheck (prop_consAppend :: String -> String -> Bool)

prop_appendEmpty :: Eq a => [[a]] -> Bool
prop_appendEmpty xs = (foldr (++) [] xs) == concat xs

testAppendEmpty :: IO ()
testAppendEmpty = quickCheck (prop_appendEmpty :: [[Int]] -> Bool)

-- 10)

-- it's fine that this test fails
prop_length :: Int -> [a] -> Bool
prop_length n xs = length (take n xs) == n

testLength :: IO ()
testLength = quickCheck (prop_length :: Int -> [String] -> Bool)

-- 11)

prop_showRead :: (Eq a, Read a, Show a) => a -> Bool
prop_showRead x = (read (show x)) == x

testShowRead :: IO ()
testShowRead = quickCheck (prop_showRead :: String -> Bool)

-- idempotence

twice :: (a -> a) -> a -> a
twice f = f . f

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

capitaliseWord :: String -> String
capitaliseWord []     = []
capitaliseWord (x:xs) = toTitle x : xs

twiceIdempotence :: String -> Bool
twiceIdempotence x = (capitaliseWord x == twice capitaliseWord x)
 && (capitaliseWord x == fourTimes capitaliseWord x)

 -- 1) 

testTwiceIdempotence :: IO ()
testTwiceIdempotence = quickCheck (twiceIdempotence)

-- 2)

prop_sorted :: (Eq a, Ord a) => [a] -> Bool
prop_sorted xs = (sort xs == twice sort xs) && (sort xs == fourTimes sort xs)   

testSorted :: IO ()
testSorted = quickCheck (prop_sorted :: [Int] -> Bool)

-- Make a Gen random generator for the datatype

-- 1) equal probabilities for each
data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

genFoolEqual :: Gen Fool 
genFoolEqual = do
  oneof [return Fulse, return Frue]

-- 2) 2/3 chance of Fulse, 1/3 chance of Frue

genFoolNonEqual :: Gen Fool
genFoolNonEqual = do
  frequency [(1, return Frue), (2, return Fulse)]
