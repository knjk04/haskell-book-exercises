module Person where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

type ValidatePerson a =
  Either [PersonInvalid] a

-- smart constructor
-- mkPerson :: Name -> Age -> Maybe Person
-- mkPerson name age
--   | name /= "" && age >= 0 =
--     Just $ Person name age
--   | otherwise = Nothing
mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age >= 0 =
    Right $ Person name age
  | name == "" = Left NameEmpty
  | otherwise = Left AgeTooLow

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
  True -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
  True -> Right name
  False -> Left [NameEmpty]
