module Ch7Exercises where

----- * grab bag exercises * -----

-- part a)

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

-- part b)

addFive = \x y -> (if x > y then y else x) + 5

-- part c)

mflip f x y = f y x

----- * variety pack * -----

-- q2

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))

----- * case practice * -----

-- q1

functionC x y = case x > y of
  True -> x
  False -> y

ifEvenAdd2 n = case even n of
  True -> n + 2
  False -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0
  


