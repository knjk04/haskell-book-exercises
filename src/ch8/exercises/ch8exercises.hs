module Ch8Exercises where

import Data.List (intersperse)

----- *** recursion *** ---

-- q3

mult :: Integral a => a -> a -> a
mult 1 n = n
mult m 1 = m
mult m n = m + mult m (n - 1)

----- *** fixing dividedBy *** -----

data DividedResult =
  Result Integer
  | DividedByZero deriving Show

dividedByFixed :: Integer -> Integer -> DividedResult
dividedByFixed _ 0 = DividedByZero
dividedByFixed n m = Result (n `div` m)

----- *** McCarthy 91 function *** -----

mc91 :: Integral a => [a] -> [a]
mc91 []       = []
mc91 (n : ns) = if n > 100 then (n - 10) :  mc91 ns else 91 : mc91 ns

----- *** Numbers into words *** -----

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits n
  | div n 10 == 0 = [n]
  | otherwise = digits (div n 10) ++ [mod n 10]

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n 

