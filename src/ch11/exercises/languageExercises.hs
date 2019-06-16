module LanguageExercises where

import Data.Char

-- 1) capitalise a word
capitaliseWord :: String -> String
capitaliseWord []     = []
capitaliseWord (x:xs) = toTitle x : xs

-- 2) capitalise sentences in a paragraph. Have to reuse capitaliseWord
capitaliseParagraph :: String -> String
capitaliseParagraph []     = []
-- capitaliseParagraph (x:xs) = (toTitle x) : capitaliseParagraph' xs
capitaliseParagraph (x:xs) = capitaliseWord [x] ++ capitaliseParagraph' xs

capitaliseParagraph' :: String -> String
capitaliseParagraph' []                     = []
capitaliseParagraph' (x:[])                 = [x]
capitaliseParagraph' (x:y:[])               = x : [y]
capitaliseParagraph' (x:y:z:xs) | x == '.'  = x : y : toTitle z : capitaliseParagraph' xs
                                | otherwise = x : capitaliseParagraph' (y:z:xs)
