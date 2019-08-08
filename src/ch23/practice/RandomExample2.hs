module RandomExample2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
-- import RandomExample -- doesn't work

-- the following 2 functions should have been imported from RandomExample, but the import didn't work
-- six-sided die
data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

-- intToDie :: Int -> Die
-- intToDie n =
--   case n of
--     1 -> DieOne
--     2 -> DieTwo
--     3 -> DieThree
--     4 -> DieFour
--     5 -> DieFive
--     6 -> DieSix
--   -- use 'error' extremely sparingly
--   x ->
--     error $
--       "intToDie got non 1-6 integer"
--       ++ show x
    
intToDie :: Int -> Die
intToDie n |
    n == 1 = DieOne
  | n == 2 = DieTwo
  | n == 3 = DieThree
  | n == 4 = DieFour
  | n == 5 = DieFive
  | n == 6 = DieSix
  | otherwise = error $ "intToDie got non 1-6 integer: " ++ show n

-- state :: Monad m => (s -> (a, s)) -> StateT s m a
-- state takes a State-like function and embeds it in the State monad transformer

-- factor out the generation of one Die
-- renamed to rollDie1' from rollDie to avoid naming conflicts
rollDie1 :: State StdGen Die
rollDie1 = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie1' :: State StdGen Die
rollDie1' =
  intToDie <$> state (randomR (1, 6))

rollDieThreeTimes'
  :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie1 rollDie1 rollDie1

-- repeat :: a -> [a]
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie1 
