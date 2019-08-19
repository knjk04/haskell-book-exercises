module UnitOfSuccess where

import Text.Trifecta

-- Prelude> parseString unit mempty "123"
-- Success 123
unit :: Parser Integer
unit = integer <* eof
