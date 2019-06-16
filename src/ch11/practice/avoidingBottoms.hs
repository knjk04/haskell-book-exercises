module AvoidingBottoms where

data ThereYet =
  There Float Int Bool
  deriving (Eq, Show)

nope = There

notYet :: Int -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet 10

-- (I did not come up with the naming)
yusssss :: ThereYet
yusssss = notQuite False


