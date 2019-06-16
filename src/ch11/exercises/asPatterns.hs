module AsPatterns where

import Data.Char

-- 1) return True if and only if all the values in the first list appear in the second list
-- (do not need to appear in the same order)
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _                          = True
isSubseqOf _ []                          = False
isSubseqOf one@(x:xs) (y:ys) | x == y    = isSubseqOf xs ys
                             | otherwise = isSubseqOf one ys

-- split a sentence into words and then tuple every word with the second element of every tuple
-- being the same as the first word but with the first letter capitalised
capitaliseWords :: String -> [(String, String)]
capitaliseWords xs = [ (word, (toTitle $ head word) : drop 1 word) | word <- words xs]
