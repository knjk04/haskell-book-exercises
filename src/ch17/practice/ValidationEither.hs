module ValidationEither where

import Data.Validation

data Errors =
    DividedByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)

-- need to fix this error

success = Success (+1)
        <*> Success 1
-- success == Success 2

failure = Success (+1)
        <*> Failure [StackOverflow]
-- failure == Failure [StackOverflow]

failure' = Failure [StackOverflow]
         <*> Success (+1)
-- failure' == Failure [StackOverflow]

failures =
     Failure [MooglesChewedWires]
 <*> Failure [StackOverflow]
-- failures == Failures [MooglesCheWedWires
--                     , StackOverflow]
