module ChapterExercises where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- zip x and y using 4 as the lookup key (will return Nothing)
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> Maybe (Integer, Integer)
x3 n = (,) <$> l <*> l
  where l = z' n

-- need to use uncurry
-- first argument is a function, addition in this case
summed :: Num c => (c, c) -> c
summed = uncurry (+)

-- lifts a boolean function over two partially applied functions
bolt :: Integer -> Bool
bolt n = (n > 3) && (n < 8)

main :: IO ()
main = do
  -- print $
  --   sequenceA [Just 3, Just 2, Just 1]

  -- print $ sequenceA [x, y]
  -- print $ sequenceA [xs, ys]
  -- print $ sequenceA [(>3), (<8), even] 7

  -- print $ summed <$> ((,) <$> xs <*> ys)
  -- print $ fmap summed ((,) <$> xs <*> zs)
  -- print $ bolt 7
  -- print $ fmap bolt z

  print $ foldr (&&) True (sequA 6) -- q1
  print $ foldr (&&) True (sequA 7) -- q1

  print $ sequA (fromMaybe 0 s') -- q2
  print $ bolt (fromMaybe 0 ys) -- q3

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)
