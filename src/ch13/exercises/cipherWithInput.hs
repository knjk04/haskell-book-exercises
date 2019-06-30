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

---------- Caesar cipher ----------

caesar :: Int -> String -> IO String  
caesar n xs = return $ caesar' (n - 1) xs

caesar' :: Int -> String -> String
caesar' _ []     = []
caesar' 0 xs     = xs
caesar' n (x:xs) = wrap n x : caesar' n xs

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

runCaesar :: IO String
runCaesar = do
  hSetBuffering stdout NoBuffering
  putStr "Enter the number of characters you want to shift by: "
  shiftBy <- getLine
  putStr "Enter some plaintext: "
  plaintext <- getLine
  caesar (read shiftBy :: Int) plaintext


