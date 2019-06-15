module ChapterExercises where

stops = "pbtkdkg"

vowels = "aeiou"

-- a) make 3-tuples of all possible stop-vowel-stop combinations
allCombinations :: [(Char, Char, Char)]
allCombinations = [(s1,v,s2) | s1 <- stops, s2 <- stops, v <- vowels]

-- b) make 3-tuples of all possible stop-vowel-stop combinations beginning with p
filterP :: [(Char, Char, Char)]
filterP = [('p', v, s2) | s2 <- stops, v <- vowels]

nouns = ["mother", "father", "baby", "child", "toddler", "teenager", "grandmother", "student",
         "teacher", "minister", "businessperson", "salesclerk", "woman", "man"]

verbs = ["ask", "be", "become", "begin", "call", "can", "come", "could", "do", "feel", "find",
         "get", "give", "go", "have", "hear", "help", "keep"]

-- c) tuples of noun-verb-noun sentences
nounVerbCombinations :: [(String, String, String)]
nounVerbCombinations = [(n1, v, n2) | n1 <- nouns, n2 <- nouns, v <- verbs]

seekritFunc2 :: Fractional a => String -> a
seekritFunc2 x = (fromIntegral $ sum (map length (words x))) / (fromIntegral $ length (words x))

-- Rewriting functions using folds

-- 1)
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2)
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> f a || b) False 
-- myAny f = foldr (\a b -> if f a then True else b) False

-- 3 (a) myElem using folding
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a b -> a == x || b) False

-- 3 (b) myElem using any
myElemAny :: Eq a => a -> [a] -> Bool
myElemAny x ys = any (== x) ys

-- 4)
myReverse :: [a] -> [a]
myReverse = foldr (\a b -> b ++ [a]) []

-- 5) write myMap using a fold. It should behave the same as map
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []
-- myMap f = foldr (\a b -> f a : b) []

-- 6) write myFilter using a fold. It should behave the same as filter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\a b -> if p a then a : b else b) []

-- 7) concat using a fold
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8) map a function over a list and concatenate the result
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

-- 9) concat using a fold and squishMap
squishAgain :: [[a]] -> [a]
squishAgain = foldr (\a b -> squishMap (:b) a) []

-- 10) take a comparison function a list and returns the greatest element in the list based on the
--     value that the comparison returned GT for
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:y:[]) = if f x y == GT then x else y
myMaximumBy f (x:y:xs) = if f x y == GT then myMaximumBy f (x:xs) else myMaximumBy f (y:xs)

-- 11) take a comparison function a list and returns the least element in the list based on the
--     value that the comparison returned LT for
-- myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
-- myMinimumBy f (x:y:[]) = if f x y == LT then x else y
-- myMinimumBy f (x:y:xs) = if f x y == LT then myMaximumBy f (x:xs) else myMaximumBy f (y:xs)


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl (\a b -> if f a b == LT then a else b) x xs
