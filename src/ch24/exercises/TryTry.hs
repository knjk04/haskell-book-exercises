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

integer4 = "(-17)"

virtuousFraction = do
  numerator <- decimal
  char '/'

  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

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

-- data IntegerOrFraction =
--     IntegerOrFraction Integer
--   | IntegerOrFraction Rational

type IntegerOrFraction =
  -- Either Integer Rational
  Either Rational Integer 


parseIoF :: Parser IntegerOrFraction
parseIoF =
  skipMany (oneOf "\n")
  >>
  --     (Left <$> integer)
  -- <|> (Right <$> virtuousFraction) 
      (Left <$> virtuousFraction)
  <|> (Right <$> integer) 

main = do
  let p f i =
        parseString f mempty i

  print $ p parseIoF frac1
  print $ p parseIoF frac2

  print $ p parseIoF eitherOr

  -- print $ p parseIoF integer1
  -- print $ p parseIoF integer2
  -- print $ p parseIoF integer3
  -- print $ p parseIoF integer4

