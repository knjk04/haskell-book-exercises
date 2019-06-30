module CipherWithInput where

import Data.Char
import System.IO (BufferMode (NoBuffering),
                 hSetBuffering,
                 stdout)
-- simple Vigenère cipher for lowercase strings (converts a string to lowercase)

type Key = String
key :: String
-- key = "haskell"
key = "ally"

aInt :: Int
aInt = ord 'a' -- = 97

zInt :: Int
zInt = ord 'z' -- = 122

vigenereEncode :: String -> Key -> String
vigenereEncode [] _          = []
vigenereEncode xs []         = vigenereEncode xs key
vigenereEncode (x:xs) (k:ks) = rightShift x k : vigenereEncode xs ks

-- 1st argument: char from input String
-- 2nd argument: char from key String
-- *** DOESN'T WORK ***
rightShift :: Char -> Char -> Char
rightShift x k = chr $ if (ord x + shiftBy) > zInt then (aInt + ((ord x + shiftBy) - zInt) - 1)
                       else ord x + shiftBy
  where shiftBy = getKeyShift k

-- tells you how much to shift by
-- *** WORKS ***
getKeyShift :: Char -> Int
getKeyShift x = ord x - ord (head key)

-- *** WORKS ***
runVigenere :: String -> IO String
runVigenere (x:xs) = return $ (toLower x) : vigenereEncode (map toLower withoutSpaces) (tail key)
  where withoutSpaces = concat $ words xs

main :: IO String
main = do
  hSetBuffering stdout NoBuffering
  putStr "Enter in a message to encrypt: "
  plaintext <- getLine
  runVigenere plaintext
