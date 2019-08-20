{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Ini where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as IO
import Test.Hspec

import Text.RawString.QQ
import Text.Trifecta

headerEx :: ByteString
headerEx = "[blah]"

-- "[blah] -> Section "blah"
newtype Header =
  Header String
  deriving (Eq, Ord, Show)

-- "brackets will be parsed then discarded, but the p will remain as our result"
parseBracketPair :: Parser a -> Parser a
parseBracketPair p =
  char '[' *> p <* char ']'

-- letter parses one character
-- some letter parses one or more characters
parseHeader :: Parser Header
parseHeader =
  parseBracketPair (Header <$> some letter)

assignmentEx :: ByteString
assignmentEx = "woot=1"

type Name = String
type Value = String
type Assignment = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  -- discard the '=' used as a delimeter between keys and values
  _ <- char '='
  -- parse 1+ characters providing they are not newlines
  val <- some (noneOf "\n")
  -- skip "end-of-line" until we receiven o more newline characters
  skipEOL -- "important!"
  return (name, val)

-- | "Skip end of line and whitespace beyond"
skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

-- no skipEOL
parseAssignment' :: Parser (Name, Value)
parseAssignment' = do
  name <- some letter
  _ <- char '='
  val <- some (noneOf "\n")
  return (name, val)
