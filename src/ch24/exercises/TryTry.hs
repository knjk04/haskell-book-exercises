module TryTry where

import Text.Trifecta
import Data.Ratio ((%))

-- test inputs
badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

-- data IntegerOrFraction =
--     IntegerOrFraction Integer
--   | IntegerOrFraction Rational

type IntegerOrFraction =
  Either Integer Rational


parseIoF :: Parser IntegerOrFraction
parseIoF =
      (Left <$> integer)
  <|> (Right <$> parseFraction) 
