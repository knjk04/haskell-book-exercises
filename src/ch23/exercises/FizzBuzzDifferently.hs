module FizzBuzzDifferently where

import Control.Monad
import Control.Monad.Trans.State

-- fizzBuzz :: Integer -> String
-- fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
--            | n `mod` 5  == 0 = "Buzz"
--            | n `mod` 3  == 0 = "Fizz"
--            | otherwise       = show n

-- instead of changing the underlying data structure, fix the reversing FizzBuzz by
-- changing the code to the following type signature
-- still use cons when making the resulting list, but have the list come out in the
-- right order to start with by enumerating the sequence in reverse
fizzBuzzFromTo :: Integer
               -> Integer
               -> [String]
fizzBuzzFromTo from to | from == to = [""]
                       | from `mod` 15 == 0 = "FizzBuzz" : fizzBuzzFromTo (from + 1) to
                       | from `mod` 5  == 0 = "Buzz"     : fizzBuzzFromTo (from + 1) to
                       | from `mod` 3  == 0 = "Fizz"     : fizzBuzzFromTo (from + 1) to
                       | otherwise          = show from  : fizzBuzzFromTo (from + 1) to

-- mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
-- execState :: State s a -> s -> s (in Control.Monad.Trans.State)
fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list =
  execState (mapM_ addResult list) []
  
-- get :: Monad m => StateT s m s 
-- put :: Monad m => s -> StateT s m ()
-- addResult :: Integer -> State [String] () -- old signature
addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzzFromTo n (n+1)
  put $ concat (result : [xs])
  -- put (result : xs)
           
main :: IO ()
main =
  mapM_ putStrLn $
    fizzBuzzList $ enumFromThenTo 100 99 1
