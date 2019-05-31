module RegisteredUser1 where

newtype Username =
  Username String

newtype AccountNumber =
  AccountNumber Integer

-- sum constructor
data User =
   UnregisteredUser
 | RegisteredUser Username AccountNumber
