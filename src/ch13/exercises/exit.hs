module Exit where

import Control.Monad
import System.Exit (exitSuccess) 
import Data.Char (toLower)  

  -- modified to successfully exit after the user enters something that isn't a palindrome
palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

-- for sentences (e.g. "Madam Im Adam")
palindromeSentences :: IO ()
palindromeSentences = forever $ do
  line1 <- getLine
  let line = concat $ words $ map toLower line1
  case (line == reverse line) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess
