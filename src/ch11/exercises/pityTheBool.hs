module PityTheBool where

-- bring Int8 in scope
import Data.Int 

data NumberOrBool =
    Numba Int8 -- (I didn't come up with the naming)
  | BoolyBool Bool -- nor this silly name
  deriving (Eq, Show)

myNumba = Numba (-128)

