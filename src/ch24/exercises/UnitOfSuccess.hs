module UnitOfSuccess where

import Text.Trifecta
import Data.Char

-- Errors if you do parseString (integer >> eof) mempty "123abc"
-- Prelude> parseString (integer >> eof) mempty "123"
-- Success ()

-- Need to rewrite the above example so that it returns the integer it parsed instead of Success ()
-- Prelude> parseString unit mempty "123"
-- Success 123

-- unit str = if all isDigit str then Success (read str :: Integer) else undefined
-- solution below from https://github.com/CarlosMChica/HaskellBook/blob/master/chapter24/UnitOfSuccess.hs
unit :: Parser Integer
unit = integer <* eof
