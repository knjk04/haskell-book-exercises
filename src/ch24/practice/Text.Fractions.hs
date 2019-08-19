{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

-- test inputs
badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

-- decimal :: Integral a => Parser a (in the context of the functions below)
-- numerator :: Integral a => a
-- char :: Char -> Parser Char

-- (%) :: Integral a => a -> a -> GHC.Real.Ratio a
-- type Rational = GHC.Real.Ratio Integer
-- the result of (%) is a concrete Integer value
parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

-- parseString :: Parser a -> Text.Trifecta.Delta.Delta -> String -> Result a
-- Result a is the thing we wanted or an error String informing us of what went wrong
main :: IO ()
main = do
  let parseFraction' =
        parseString parseFraction mempty


  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction


  -- program halts on the error
  -- print $ parseFraction' badFraction
  -- print $ parseFraction' shouldWork
  -- print $ parseFraction' shouldAlsoWork
  -- print $ parseFraction' alsoBad

virtuousFraction :: Parser Rational
virtuousFraction = do

  numerator <- decimal
  char '/'

  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

testVirtuous :: IO ()
testVirtuous = do
  let virtuousFraction' =
        parseString virtuousFraction mempty
  
  print $ virtuousFraction' badFraction
  print $ virtuousFraction' alsoBad
  
  print $ virtuousFraction' shouldWork
  print $ virtuousFraction' shouldAlsoWork
