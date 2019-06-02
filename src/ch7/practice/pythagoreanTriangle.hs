module PythagoreanTriangle where

-- c is the hypotenuse of the triangle

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
  | a^2 + b^2 == c^2 = "RIGHT ON"
  | otherwise        = "not right"
