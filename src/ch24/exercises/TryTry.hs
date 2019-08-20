{-# LANGUAGE QuasiQuotes #-}

module TryTry where

import Control.Applicative
import Text.Trifecta
import Data.Ratio ((%))
import Text.RawString.QQ

------------------ From Text.Fractions ------------------
-- test inputs
frac1 = "1/2"
frac2 = "2/1"

integer1 = "56"

integer2 = "202"

integer3 = "0"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)


eitherOr :: String
eitherOr = [r|
11/6
123
1/2
0
1
5/6
1/1|]

--------------------------------------------------------

type IntegerOrFraction =
  -- Either Integer Rational
  Either Rational Integer 


parseIntegerOrFraction :: Parser IntegerOrFraction
parseIntegerOrFraction = try (Left <$> parseFraction) <|> (Right <$> integer) 


main = do
  let p f i =
        parseString f mempty i

  print $ p parseIntegerOrFraction eitherOr

  print $ parseString parseIntegerOrFraction mempty frac1
  print $ parseString parseIntegerOrFraction mempty frac2
  print $ parseString parseIntegerOrFraction mempty integer1
  print $ parseString parseIntegerOrFraction mempty integer2
  print $ parseString parseIntegerOrFraction mempty integer3
