module ChapterExercises where

import Data.List -- for intersperse

-- 1)
notThe :: String -> Maybe String
-- notThe [] = Nothing
notThe xs | xs == "the" = Nothing
          | otherwise   = Just xs

-- this should be a recursive function
replaceThe :: String -> String
replaceThe xs = concat $ intersperse " " replaced
  where replaced = [ if split == "the" then "a" else split | split <- words xs]

-- 2) takes a string, splits it into words, and counts then number of occurrences of the word
-- "the" followed by a word beginning with a vowel

countTheBeforeVowel :: String -> Integer  
countTheBeforeVowel xs = countTheBeforeVowel' (words xs) 

-- safe to use head and tail here
countTheBeforeVowel' :: [String] -> Integer
countTheBeforeVowel' xs
  | length xs < 2 = 0 -- handles empty list and singleton list
  | otherwise     = if (head xs) == "the" && (head $ head $ tail xs) `elem` vowels
                      then 1 + countTheBeforeVowel' (tail $ tail xs)
                    else countTheBeforeVowel' (tail xs)

countVowels :: String -> Integer
-- countVowels []     = 0
-- countVowels (x:xs) = if x `elem` "aeiou" then 1 + countVowel xs else countVowel xs
countVowels xs = foldr (\x -> if x `elem` "aeiou" then (+1) else (+0)) 0 xs

-- *** validate the word ***
-- if the number of vowels exceeds the number of constants, return Nothing in "in many human languages,
-- vowels rarely exceed the number of consonants, so when they do, it may indicate that the input
-- isn't a word"

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord xs | vowels > consonants = Nothing
          | otherwise           = Just (Word' xs)
  where vowels     = countVowels xs
        consonants = (fromIntegral $ length xs) - vowels

-- natural numbers

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

-- *** BROKEN *** 
integerToNat :: Integer -> Maybe Nat
integerToNat n | n < 0     = Nothing
               | n == 0    = Just Zero
               | otherwise = Just $ Succ (integerToNat' (n-1))
               -- | otherwise = Just $ Succ (integerToNat (n-1))

-- helper function for cases where n > 0 (first case is simply a base case since the function is
-- recursive -- the call to integerToNat' from integerToNat will not pass 0 as an argument)
integerToNat' :: Integer -> Nat
integerToNat' 0 = Zero
integerToNat' n = Succ (integerToNat' (n-1))

-- *** Small library for Maybe ***

-- 1 a)
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

-- 1 b)
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

-- 2)  Maybe catamorphism
maybeCatamorphism :: b -> (a -> b) -> Maybe a -> b
maybeCatamorphism x _ Nothing  = x
maybeCatamorphism _ f (Just y) = f y

-- 3) "provide a fallback value"
fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe _ (Just y) = y

-- 4 a) Maybe head
listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:_)  = Just x

-- 4 b) Construct a list by consing an element onto an empty list
maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

-- 5) remove Nothing values from a list
catMaybes :: [Maybe a] -> [a]
catMaybes []           = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just x:xs)  = x : catMaybes xs

-- 6) Note: I've added in the Eq constraint
flipMaybe :: Eq a => [Maybe a] -> Maybe [a]
flipMaybe xs | any (== Nothing) xs  || null xs = Nothing
             -- | otherwise                       = Just $ map (fromMaybe (head xs)) xs
             | otherwise                       = Just $ catMaybes xs

-- *** Small library for Either ***

-- 1) return a list of Left elements using foldr
-- foldr version from -- lefts' xs = foldr (\x -> if x == (Left _) then (x : []) else []) [] xs
lefts' :: [Either a b] -> [a]
-- lefts' []            = []
-- lefts' (Left x : xs) = x : lefts' xs
-- lefts' (_ : xs)      = lefts' xs
lefts' = foldr f []
  where f (Left a) xs  = a : xs
        f (Right a) xs = xs

-- 1) return a list of Right elements using foldr
rights' :: [Either a b] -> [b]        
rights' = foldr f []
  where f (Left a)  xs = xs
        f (Right a) xs = a : xs

-- 3) Split Left and Right values
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

-- 4)
-- Example usage: eitherMaybe' (+1) (Right 2)
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just $ f x
eitherMaybe' f (Left _)  = Nothing

-- 5) "general catamorphism for Either values"
-- Example usage: eitherMaybe' (+1) (*3 ) (Right 2)
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ g (Right x) = g x


-- *** write your own iterate and unfoldr *** 
-- Example usage (taken from book): Prelude> take 10 $ iterate (+1) 0
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = a : myUnfoldr f b'
  where Just (a, b') = f b

-- 3) rewrite myIterate using myUnfoldr
-- taken from https://github.com/gvolpe/haskell-book-exercises/blob/master/chapter12/exercises.hs
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\a -> Just (a, f a)) x  

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- untested
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = Node (unfold f x) b (unfold f x)
  where Just (a, b, a') = (f x)


-- tree builder using unfold
-- treeBuild :: Integer -> BinaryTree Integer
-- treeBuild 0 = Leaf
-- -- treeBuild n = Node (unfold treeBuild n) n (unfold treeBuild n)
