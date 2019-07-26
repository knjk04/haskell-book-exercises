module Main where

{-# LANGUAGE OverloadedStrings #-}
-- OverloadedStrings makes Strings polymorphic over the Num type class
-- Strings are usually a concrete type, not polymorphic
-- OverloadedStrings allows Strings to be used as Text and ByteString values

-- import Lib
import Control.Monad (repliacateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty

main :: IO ()
main = undefined
-- main = someFunc

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

-- choose a random element in the alphaNum range
randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  -- "right of arrow is IO Int,
  -- so randomDigit is Int"
  randomDigit <- SR.randomRIO (0, maxIndex)
  return (xs !! randomDigit)
  
-- apply randomElement to alphaNum to get one random letter or number
shortyGen :: IO [Char]
shortyGen =
  replicateM 7 (randomElement alphaNum)

