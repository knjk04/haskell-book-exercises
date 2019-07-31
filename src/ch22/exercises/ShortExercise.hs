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
tupled :: [Char] -> ([Char], [Char])
tupled = do
  capped <- cap
  reversed <- rev
  return (capped, reversed)

-- return results of cap and rev as a tuple
-- need to make the function monadic using >>=
tupled' :: [Char] -> ([Char], [Char])
tupled' str = (modifyStr rev, modifyStr cap)
  where modifyStr capOrRev = (pure str) >>= (pure $ capOrRev str)
