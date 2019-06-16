module Vigenere where

import Data.Char

-- simple Vigenère cipher

type Key = String
key :: String
-- key = "haskell"
key = "ally"

zInt :: Int
zInt = ord 'z'

vigenereEncode :: String -> Key -> String
vigenereEncode [] _          = []
vigenereEncode xs []         = vigenereEncode xs key
vigenereEncode (x:xs) (k:ks) = (chr $ (ord x + ord k)) : vigenereEncode xs ks

-- 1st argument: char from input String
-- 2nd argument: char from key String
rightShift :: Char -> Char -> Char
rightShift x k = chr $ ord x + (getKeyShift k)

-- tells you how much to shift by
getKeyShift :: Char -> Int
getKeyShift x = ord x - ord (head key)


runVigenere :: String -> String
runVigenere (x:xs) = x : vigenereEncode xs (tail key)
