module Lookups where

import Data.List (elemIndex)

-- Use the following terms to make the expressions typecheck:
-- 1. pure
-- 2. (<$>) or fmap
-- 3. (<*>)

-- 1)
added :: Maybe Integer
added =
  (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3)

-- elemIndex returns the index of the input element
x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y'

-- 4)
xs = [1, 2, 3]
ys = [4, 5, 6]

x4 :: Maybe Integer
x4 = lookup 3 $ zip xs ys

y4 :: Maybe Integer
y4 = lookup 2 $ zip xs ys

-- returns the 2nd element in the tuple
-- not sure if this is the expected behaviour
summed :: Maybe Integer
summed = sum <$> ((,) <$> x4 <*> y4)

