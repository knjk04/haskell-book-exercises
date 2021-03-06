{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

type NumberOrString =
  Either Integer String

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

-- eitherOr :: String
-- eitherOr = [r|123
-- abc
-- 456
-- def
-- |]

-- eitherOr :: String
-- eitherOr = [r|
-- 123
-- abc
-- 456
-- def|]

a = "blah"
b = "123"
c = "123blah789"

-- parseNos :: Parser NumberOrString
-- parseNos =
--       (Left <$> integer)
 -- <|> (Right <$> some letter)

-- parseNos :: Parser NumberOrString
-- parseNos =
--   skipMany (oneOf "\n")
--   >>
--       (Left <$> integer)
--   <|> (Right <$> some letter)

parseNos :: Parser NumberOrString
parseNos =
  skipMany (oneOf "\n")
  >>
      (Left <$> integer)
  <|> (Right <$> some letter)

main = do
  let p f i =
        parseString f mempty i

  print $ p parseNos eitherOr

  print $ p (some letter) a
  print $ p integer b

  print $ p parseNos a
  print $ p parseNos b
  -- many: 0+
  print $ p (many parseNos) c
  -- many: 1+
  print $ p (some parseNos) c
