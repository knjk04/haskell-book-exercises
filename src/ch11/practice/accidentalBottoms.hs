module AccidentalBottoms where

-- data Automobile = Null
--   | Car { make  :: String
--         , model :: String
--         , year  :: Integer }
--     deriving (Eq, Show)

-- "split out the record/product"
data Car = Car { make  :: String
               , model :: String
               , year  :: Integer }
           deriving (Eq, Show)

-- Null is not ideal, but left in to make a point
data Automobile = Null
  | Automobile Car
  deriving (Eq, Show)
