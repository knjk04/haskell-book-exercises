module Ch5Exercises where

-- Parametricity exercises

q2partOne :: a -> a -> a
q2partOne a b = a

q2partTwo :: a -> a -> a
q2partTwo a b = b

q3 :: a -> b -> b
q3 a b = b
