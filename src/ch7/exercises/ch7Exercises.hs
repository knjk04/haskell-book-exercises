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
  
----- * Let's write code * -----

-- q1 a)

tensDigit :: Integral a => a -> a
tensDigit x = xLast `mod` 10
  where (xLast, _) = x `divMod` 10

-- q1 c)

hundredsDigit :: Integral a => a -> a 
hundredsDigit x = d2
  where d  = x `div` 100
        d2 = d `mod` 10

-- q2. Have to use a case expression

foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of
  False -> x
  True -> y

-- q2. Have to use guards

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
  | b         = y
  | otherwise = x

-- q3

g :: (a -> b) -> (a, c) -> (b, c)
g aToB (a, c) = (aToB a, c)

-- q4. Must be pointfree

roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

-- q5 
roundTrip' :: (Show a, Read b) => a -> b
roundTrip' a = read (show a) 

main = do
  print (roundTrip' 4 :: Int)
  print (id 4)
