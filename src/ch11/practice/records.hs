module Records where

data Person =
  MkPerson String Int
  deriving (Eq, Show)

-- sample data
jm = MkPerson "julie" 108
ca = MkPerson "chris" 16

name :: Person -> String
name (MkPerson s _ ) = s

-- Same using record syntax

data Person2 =
  Person2 { name2 :: String,
            age2 :: Int }
  deriving (Eq, Show)

-- more sample data
jm2 = Person2 "julie" 105
ca2 = Person2 "chris" 53
