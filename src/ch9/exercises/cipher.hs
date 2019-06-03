module Cipher where

import Data.Char

caesar :: Int -> String -> String
caesar _ []     = []
caesar 0 xs     = xs
caesar n (x:xs) = wrap n x : caesar n xs

uncaesar :: Int -> String -> String
uncaesar _ []     = []
uncaesar 0 xs     = xs
uncaesar n (x:xs) = unwrap n x : uncaesar n xs

-- constants
zUnicode = 122
aUnicode = 97
finiteField = zUnicode `mod` aUnicode -- 25

wrap :: Int -> Char -> Char
wrap n x = if (ord x + n) > zUnicode then chr (zUnicode - finiteField + (n - 1) - diff)
           else chr (ord x + n)
                where diff = zUnicode - ord x

unwrap :: Int -> Char -> Char
unwrap n x = if (ord x - n) < aUnicode then chr (aUnicode + finiteField - n + diff + 1)
             else chr (ord x - n)
                  where diff = ord x - aUnicode
