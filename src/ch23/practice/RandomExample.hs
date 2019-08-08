module RandomExample where

import System.Random

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

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1 :: Int, 6) s1
      (d3, _) = randomR (1, 6) s2
  (intToDie d1, intToDie 2, intToDie d3)
