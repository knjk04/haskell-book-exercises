module ShortExercise where

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

-- 1) straightforward
composed :: [Char] -> [Char]
composed = rev . cap

-- 2) also straightforward
fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

-- 3) need to use an Applicative using do notation
-- tupled :: [Char] -> ([Char], [Char])
-- tupled xs = do
--   let capped   = cap xs
--   let reversed = rev xs
--   return (capped, reversed)
  -- return (cap xs , rev xs)
