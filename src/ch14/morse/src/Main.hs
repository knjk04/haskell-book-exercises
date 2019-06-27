module Main where

import Control.Monad (forever, when)
import Data.List (intercalate)
import Data.Traversable (traverse)
import Morse (stringToMorse, morseToChar)
import System.Environment (getArgs)
import System.Exit (exitFailure,
                    exitSuccess)
import System.IO (hGetLine, hIsEOF, stdin)

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

convertToMorse :: IO ()
convertToMorse = forever $ do
  weAreDone <- hIsEOF stdin
  when weAreDone exitSuccess

  -- otherwise, proceed
  line <- hGetLine stdin
  convertLine line
  where
    convertLine line = do
      let morse = stringToMorse line
      case morse of
        (Just str)
          -> putStrLn
             (intercalate " " str)
        Nothing
          -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

convertFromMorse :: IO ()
convertFromMorse = forever $ do
  weAreDone <- hIsEOF stdin
  when weAreDone exitSuccess

  -- otherwise, proceed
  line <- hGetLine stdin
  convertLine line
  where
    convertLine line = do
      let decoded :: Maybe String
          decoded =
            traverse morseToChar
                     (words line)
      case decoded of
        (Just s) -> putStrLn s
        Nothing  -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

main :: IO ()
main = do
  mode <- getArgs
  case mode of
    [arg] ->
      case arg of
        "from" -> convertFromMorse
        "to"   -> convertToMorse
        _      -> argError
    _ -> argError
    where argError = do
            putStrLn "Please specify the\
                     \ first argument\
                     \ as being 'from' or\
                     \ 'to' morse,\
                     \ such as: morse to"
            exitFailure

-- *** Arbitrary instances ***

-- Trivial  

data Trivial =
  Trivial
  deriving (Eq, Show)

-- return is required to lift Trivial into the Gen monad
trivialGen :: Gen Trivial
trivialGen =
  return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

-- identity crisis

data Identity a =
  Identity a
  deriving (Eq, Show)

identityGen :: Arbitrary a =>
  Gen (Identity a)

identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a =>
  Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

-- Arbitrary products

data Pair a b =
  Pair a b
  deriving (Eq, Show)

pairGen :: (Arbitrary a,
            Arbitrary b) =>
           Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance (Arbitrary a,
          Arbitrary b) =>
         Arbitrary (Pair a b) where
  arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

-- Arbitrary for sum types

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

-- oneof assigns an equal probability to every value 
sumGenEqual :: (Arbitrary a,
                Arbitrary b) =>
               Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a,
         return $ Second b]
  
sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

-- arbitrary Just value is 3x more likely than a Nothing value
-- instance Arbitrary a =>
--          Arbitrary (Maybe a) where
--   arbitrary =
--     frequency [(1, return Nothing),
--                (3, liftM Just arbitrary)]

sumGenFirstPls :: (Arbitrary a,
                   Arbitrary b) =>
                  Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a),
             (1, return $ Second b)]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls
