module KleisliComposition where

import Control.Monad ((>=>))

-- (>=>) is called the Kleisli composition (operator ?)
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

-- flip (.) :: (a -> b) -> (b -> c) -> a -> c

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

-- return lifts the result of read into a Monadic structure
readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge =
  getAge "How old are you?"
