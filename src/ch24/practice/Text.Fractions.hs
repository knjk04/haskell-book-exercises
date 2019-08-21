{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta

-- test inputs
badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

-- decimal :: Integral a => Parser a (in the context of the functions below)
-- numerator :: Integral a => a
-- char :: Char -> Parser Char

-- (%) :: Integral a => a -> a -> GHC.Real.Ratio a
-- type Rational = GHC.Real.Ratio Integer
-- the result of (%) is a concrete Integer value
-- parseFraction :: Parser Rational
parseFraction :: (Monad m, TokenParsing m)
              => m Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  -- return (numerator % denominator)
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

-- parseString :: Parser a -> Text.Trifecta.Delta.Delta -> String -> Result a
-- Result a is the thing we wanted or an error String informing us of what went wrong
main :: IO ()
main = do
  -- "parseOnly is Attoparsec"
  let attoP = parseOnly parseFraction

  print $ attoP badFraction
  print $ attoP shouldWork
  print $ attoP shouldAlsoWork
  print $ attoP alsoBad

  -- "parseString is Trifecta"
  let p f i =
        parseString f mempty i

  print $ p parseFraction badFraction
  print $ p parseFraction shouldWork
  print $ p parseFraction shouldAlsoWork
  print $ p parseFraction alsoBad

  -- let parseFraction' =
  --       parseString parseFraction mempty

  -- print $ parseFraction' shouldWork
  -- print $ parseFraction' shouldAlsoWork
  -- print $ parseFraction' alsoBad
  -- print $ parseFraction' badFraction


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
