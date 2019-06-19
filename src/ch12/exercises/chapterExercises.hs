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

-- *** BROKEN ***   
countTheBeforeVowel :: String -> Integer  
countTheBeforeVowel xs = countTheBeforeVowel' (words xs) 

-- safe to use head and tail here
countTheBeforeVowel' :: [String] -> Integer
countTheBeforeVowel' xs
  | length xs < 2 = 0 -- handles empty list and singleton list
  | otherwise     = if (head xs) == "the" && (head $ head $ tail xs) `elem` "aeiou"
                      then 1 + countTheBeforeVowel' (tail $ tail xs)
                    else countTheBeforeVowel' (tail xs)

countVowel :: String -> Integer
-- countVowel []     = 0
-- countVowel (x:xs) = if x `elem` "aeiou" then 1 + countVowel xs else countVowel xs
countVowel xs = foldr (\x -> if x `elem` "aeiou" then (+1) else (+0)) 0 xs
