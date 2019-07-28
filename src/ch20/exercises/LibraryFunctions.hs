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

-- minimum' :: (Foldable t, Ord a) => t a -> Maybe a
-- minimum' n = foldMap (\x -> min x n == n) n
-- minimum' n = foldr (\x -> if min x n == n then Just else Just) Just n
-- minimum' n = foldMap (\x -> if min x n == n then Just else Just) n
-- minimum' = undefined

-- minimum' xs = foldMap min xs
-- minimum' xs = foldMap (\x y -> if x > y then Just x else Just y) xs

-- minimum' xs
--   | null xs = Nothing
--   | otherwise = foldMap (\x y -> if x > y then Just x else Just y) xs

-- minimum' xs =

-- 5) maximum :: (Foldable t, Ord a) => t a -> Maybe a

-- 6) null :: (Foldable t) => t a -> Bool
null' :: (Foldable t) => t a -> Bool
null' xs = getAll (foldMap (\_ -> All False) xs :: All)

-- 7) length :: (Foldable t) => t a -> Int
length' :: (Foldable t) => t a -> Int
length' xs = getSum (foldMap (\_ -> 1) xs :: Sum Int)

-- 8) toList :: (Foldable t) => t a -> [a]
toList' :: (Foldable t) => t a -> [a]
-- toList' x = foldr (:) [] x -- this also works
toList' x = foldMap (: []) x

-- 9) Hint: use foldMap
-- | Combine the elements
--   of a structure using a monoid
-- fold :: (Foldable t, Monoid m) => t m -> m

-- Prelude> xs = map Sum [1..5]
-- Prelude> fold' xs 
fold' :: (Foldable t, Monoid m) => t m -> m
fold' xs = foldMap (<> mempty) xs

-- 10) define foldMap using foldr
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- foldMap' f ta = foldr f mempty ta
-- foldMap' f ta = foldr (<> f) mempty ta
foldMap' = undefined


foldMap' f ta = foldMap f ta -- works but need to use foldr
