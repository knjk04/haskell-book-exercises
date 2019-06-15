module ScansExercises where

fibs = 1 : scanl (+) 1 fibs

fibsN x = fibs !! x

-- q1: change fibs to return the first 20 Fibonacci numbers
twentyFibs = take 20 $ 1 : scanl (+) 1 twentyFibs

-- q2: change fibs to return Fibonacci numbers that are less than 100
underHundredFibs = takeWhile (< 100) $ 1 : scanl (+) 1 underHundredFibs

-- q3 write factorial from recursion as a scan
-- note: this generates an infinite list, so needs to be passed through, e.g., take
factorialScan = scanl (*) 1 [1..]
