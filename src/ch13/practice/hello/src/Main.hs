module Main where

import Hello
import DogsRule
import System.IO

main :: IO ()
main = do
  -- putStrLn "hello world"
  hSetBuffering stdout NoBuffering
  putStr "Please enter your name: "
  name <- getLine
  sayHello name
  dogs