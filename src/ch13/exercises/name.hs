module Name where

import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout) 

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age | name /= "" && age > 0 = Right $ Person name age
                  | name == ""            = Left NameEmpty
                  | age < 0               = Left AgeTooLow
                  | otherwise =
                    Left $ PersonInvalidUnknown $
                    "Name was " ++ show name ++
                    " Age was " ++ show age


-- not yet working                  
gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "Please enter your name: "
  name <- getLine
  putStr "Please enter your age: "
  age <- getLine
  case mkPerson name (read age :: Integer) of
    Left (PersonInvalidUnknown errorMsg) -> errorMsg
    Right (Person name age) -> return $ "Yay! Successfully got a person: " ++
      name ++ " aged " ++ show age

  return ()
                    
