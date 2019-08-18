module UnitOfSuccess where

import Text.Trifecta

-- Prelude> parseString unit mempty "123"
-- Success 123

-- unit y@(x:xs) = integer >> eof 

-- unit :: String -> Parser String
-- unit = mapM char

-- unit y@(x:xs) = integer >> eof >> Success ()

-- unit :: String -> Parser Integer
-- unit = mapM integer

-- unit :: String -> Parser Integer
-- unit = integer 
