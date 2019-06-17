module Person where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

-- smart constructor
mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0 =
    Just $ Person name age
  | otherwise = Nothing
