module LibraryFunctions where

import Data.Monoid

-- write the functions using foldMap or foldr from Foldable then try them out with multiple types
-- that have Foldable instances

-- foldMap "can take a function to map that is different from the Monoid it is using"
-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- 1) Nicer with foldMap, but can use foldr, too
-- sum :: (Foldable t, Num a) => t a -> a

-- Prelude> xs = map Sum [1..4]
-- Prelude> sum' xs
sum' :: (Foldable t, Num m, Monoid m) => t m -> m
sum' n = foldMap (+0) n

-- 2) Also nicer with foldMap
-- product :: (Foldable t, Num a) => t a -> a
-- Prelude> xs = map Product [1..4]
-- Prelude> sum' xs
product' :: (Foldable t, Num m, Monoid m) => t m -> m
product' n = foldMap (*1) n

-- 3) elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' y xs = getAny ( foldMap (\x -> Any $ x == y) xs :: Any)

-- 4) minimum :: (Foldable t, Ord a) => t a -> Maybe a

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
-- minimum' n = foldMap (\x -> min x n == n) n
-- minimum' n = foldr (\x -> if min x n == n then Just else Just) Just n
-- minimum' n = foldMap (\x -> if min x n == n then Just else Just) n
-- minimum' = undefined

