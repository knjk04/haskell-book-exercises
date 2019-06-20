module ChapterExercises where

import Data.List -- for intersperse

-- 1)
notThe :: String -> Maybe String
-- notThe [] = Nothing
notThe xs | xs == "the" = Nothing
          | otherwise   = Just xs

-- this should be a recursive function
replaceThe :: String -> String
replaceThe xs = concat $ intersperse " " replaced
  where replaced = [ if split == "the" then "a" else split | split <- words xs]

-- 2) takes a string, splits it into words, and counts then number of occurrences of the word
-- "the" followed by a word beginning with a vowel

countTheBeforeVowel :: String -> Integer  
countTheBeforeVowel xs = countTheBeforeVowel' (words xs) 

-- safe to use head and tail here
countTheBeforeVowel' :: [String] -> Integer
countTheBeforeVowel' xs
  | length xs < 2 = 0 -- handles empty list and singleton list
  | otherwise     = if (head xs) == "the" && (head $ head $ tail xs) `elem` vowels
                      then 1 + countTheBeforeVowel' (tail $ tail xs)
                    else countTheBeforeVowel' (tail xs)

countVowels :: String -> Integer
-- countVowels []     = 0
-- countVowels (x:xs) = if x `elem` "aeiou" then 1 + countVowel xs else countVowel xs
countVowels xs = foldr (\x -> if x `elem` "aeiou" then (+1) else (+0)) 0 xs

-- *** validate the word ***
-- if the number of vowels exceeds the number of constants, return Nothing in "in many human languages,
-- vowels rarely exceed the number of consonants, so when they do, it may indicate that the input
-- isn't a word"

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord xs | vowels > consonants = Nothing
          | otherwise           = Just (Word' xs)
  where vowels     = countVowels xs
        consonants = (fromIntegral $ length xs) - vowels

-- natural numbers

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

-- *** BROKEN *** 
integerToNat :: Integer -> Maybe Nat
integerToNat n | n < 0     = Nothing
               | n == 0    = Just Zero
               | otherwise = Just $ Succ (integerToNat' (n-1))
               -- | otherwise = Just $ Succ (integerToNat (n-1))

-- helper function for cases where n > 0 (first case is simply a base case since the function is
-- recursive -- the call to integerToNat' from integerToNat will not pass 0 as an argument)
integerToNat' :: Integer -> Nat
integerToNat' 0 = Zero
integerToNat' n = Succ (integerToNat' (n-1))
