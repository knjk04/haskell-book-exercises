module RegisteredUser2 where

newtype Username =
  Username String

newtype AccountNumber =
  AccountNumber Integer

-- sum constructor
data User =
   UnregisteredUser
 | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser =
  putStrLn "UnregisteredUser"
printUser (RegisteredUser
            (Username name)
            (AccountNumber acctNum)) =
  putStrLn $ name ++ " " ++ show acctNum
