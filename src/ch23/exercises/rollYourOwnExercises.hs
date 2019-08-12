module RollYourOwnExercises where

import System.Random

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)
    
intToDie :: Int -> Die
intToDie n |
    n == 1 = DieOne
  | n == 2 = DieTwo
  | n == 3 = DieThree
  | n == 4 = DieFour
  | n == 5 = DieFive
  | n == 6 = DieSix
  | otherwise = error $ "intToDie got non 1-6 integer: " ++ show n

-- 1) "refactor rollsToGetTwenty into having the limit be a function argument"

-- mkStdGen :: Int -> StdGen
-- next : g -> (Int, g) where g :: StdGen
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n gen = go 0 0 gen
  where
    go sum count gen
      | sum >= n = count
      | otherwise =

        let (die, nextGen) =
              randomR (1, 6) gen
        in go (sum + die)
              (count + 1) nextGen

-- rollsToGetTwenty :: StdGen -> Int
-- rollsToGetTwenty g = go 0 0 g
--   where
--     go :: Int -> Int -> StdGen -> Int
--     go sum count gen
--       | sum >= 20 = count
--       | otherwise =

--         let (die, nextGen) =
--               randomR (1, 6) gen
--         in go (sum + die)
--               (count + 1) nextGen

-- 2) "Change rollsToGetN to record the series of die that occurred in addition to the count"
rollsCountLogged :: Int
                 -> StdGen
                 -> (Int, [Die])
rollsCountLogged n gen = go 0 0 gen []
  where
    go sum count gen recordDie
      | sum >= n = (count, recordDie)
      | otherwise =

        let (die, nextGen) =
              randomR (1, 6) gen
        in go ((count + 1) nextGen (recordDie ++ [intToDie die])

