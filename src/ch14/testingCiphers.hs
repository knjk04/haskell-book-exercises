module TestingCiphers where

import Test.QuickCheck
import CipherWithInput

prop_caesarUnCaesar :: Int -> String -> Bool
prop_caesarUnCaesar n xs = encrypted == uncaesar n encrypted
  where encrypted = caesar' n xs

-- fails on 1 "a", not sure why. Works on ghci when I manually test it
test_caesar :: IO ()
test_caesar = quickCheck prop_caesarUnCaesar

-- test vigenere after I've written a decrypt function
