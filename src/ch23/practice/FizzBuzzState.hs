module FizzBuzzState where

import Control.Monad
import Control.Monad.Trans.State
-- http://hackage.haskell.org/package/dlist
import qualified Data.DList as DL


fizzBuzz :: Integer -> State
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

fizzBuzzList :: [Integer]
             -> DL.DList String
fizzBuzzList list =
  execState (mapM_ addResult list) DL.empty
  
-- State is a type alias of StateT
addResult :: Integer
          -> State (DL.DList String) ()
addResult n = do
    xs <- get
    let result = fizzBuzz n 
    -- snoc (get it?!) appends to the end, unlike cons which adds to the front
    put (DL.snoc xs result)
  
main :: IO ()
main =
  mapM_ putStrLn $ fizzBuzzList [1..100]
