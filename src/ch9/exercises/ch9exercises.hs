module Ch9Exercises where

import Data.Char
import Data.List (intersperse)
import Data.Bool

eftBool :: Bool -> Bool -> [Bool]
eftBool True False  = []
eftBool False True  = [False, True]
eftBool b _         = [b] -- handles the case where the booleans are the same value
-- eftBool True True   = [True]
-- eftBool False False = [False]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd o o' | o > o'    = []
            | o == o'   = [o]
            | otherwise = case o of
                            EQ -> [o, o'] -- [EQ, GT]
                            LT -> if o' == GT then [o, EQ, o'] else [o, o']

eftInt :: Int -> Int -> [Int]
eftInt n m | n > m     = []
           | otherwise = n : eftInt (n + 1) m


eftChar :: Char -> Char -> [Char]
eftChar c c' | c > c'    = []
             | otherwise = c : eftChar (chr (ord c + 1)) c'

----- *** Thy Fearful Symmetry *** -----

-- q1 (have to use takeWhile and dropWhile)

-- myWords :: String -> [String]
-- myWords [] = []
-- myWords str = beforeSpace : myWords (drop 1 $ dropWhile (/= ' ') str)
--   where beforeSpace = takeWhile (/= ' ') str

-- q3 version of myWords

myWords :: Char -> String -> [String]
myWords _ []          = []
myWords separator str = beforeSpace : myWords separator (drop 1 $ dropWhile (/= separator) str)
  where beforeSpace = takeWhile (/= separator) str

----- *** q2 (PoemLines) *** -----

firstSen  = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen  = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
            \ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

-- myLines :: String -> [String]
-- myLines []  = []
-- myLines str = takeWhile (/= '\n') str : myLines (drop 1 $ dropWhile (/= '\n') str)

-- q3 variant of myLines

myLines :: Char -> String -> [String]
myLines _ []          = []
myLines separator str =
  takeWhile (/= separator) str : myLines separator (drop 1 $ dropWhile (/= separator) str)

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $
  "Are they equal? "
  -- ++ show (myLines sentences
  ++ show (myLines '\n' sentences
           == shouldEqual)

----- *** Square Cube *** -----

mySqr :: [Integer]
mySqr = [x^2 | x <- [1..5]]

myCube :: [Integer]
myCube = [y^3 | y <- [1..5]]

-- q1

mySqrCube :: [(Integer, Integer)]
mySqrCube = [(x,y) | x <- mySqr, y <- myCube]

-- q2

mySqrCube2 :: [(Integer, Integer)]
mySqrCube2 = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]

mySqrCube2Len :: Int
mySqrCube2Len = length mySqrCube2

----- *** Exercises: More Bottom *** -----

q6 :: a -> a -> [Bool] -> [a]
q6 a b xs = map (bool a b) xs

----- *** Filtering *** -----

-- q1

mult3 :: Integral a => [a] -> [a]
mult3 ns = filter (\x -> (x `rem` 3) == 0) ns

-- q2

mult3Len :: Integral a => [a] -> Int
mult3Len = length . mult3  

-- q3 

filterArticles :: String -> [String]
filterArticles str = filter (\x -> x /= "the" && x /= "a" && x /= "an") (words str)

----- *** Zipping exercises *** -----

-- q1

myZip :: [a] -> [b] -> [(a,b)]
myZip [] _          = []
myZip _ []          = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

-- q2

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _            = []
myZipWith _ _ []            = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

-- q3

myZip2 :: [a] -> [b] -> [(a,b)]
myZip2 xs ys = myZipWith (,) xs ys

----- *** 9.12 Chapter exercises *** -----

-- q2

filterUpper :: String -> String
filterUpper xs = filter isUpper xs

-- q3

capitalise :: String -> String
capitalise [] = []
capitalise (x:xs) = if isUpper x then x:xs else toUpper x : xs

-- q4

allCaps :: String -> String
allCaps [] = []
allCaps (x:xs) = toUpper x : allCaps xs

-- q5

uppercaseOne :: String -> Char
uppercaseOne xs = toUpper $ head xs

-- q6

uppercaseOneComposed :: String -> Char
uppercaseOneComposed xs = toUpper . head $ xs

-- q7

uppercaseOnePointfree :: String -> Char
uppercaseOnePointfree = toUpper . head

----- *** writing your own standard functions *** -----

-- q1

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

-- q2

myAny :: (a -> Bool) -> [a] -> Bool
myAny f []     = False
myAny f (x:xs) = f x || myAny f xs

-- q3

myElem :: Eq a => a -> [a] -> Bool
myElem x []                 = False
myElem x (y:ys) | x == y    = True
                | otherwise = myElem x ys

-- q4
myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x ys = any (== x) ys

-- q5

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- q6

squish :: [[a]] -> [a]
squish []         = []
squish ((xs):xss) = xs ++ squish xss

-- q6

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f []     = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- q7 (have to use squishMap)

squishAgain :: [[a]] -> [a]
squishAgain []       = []
squishAgain (xs:xss) = squishMap (:[]) xs ++ squishAgain xss

-- q8

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x:[])    = x -- singleton list means x is the 'last man standing' (GT than the others)
myMaximumBy f (x:y:xss) = case f x y of
  GT -> myMaximumBy f (x:xss)
  _  -> myMaximumBy f (y:xss)

-- q9

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ (x:[])    = x -- singleton list means x is the 'last man standing' (GT than the others)
myMinimumBy f (x:y:xss) = case f x y of
  LT -> myMinimumBy f (x:xss)
  _  -> myMinimumBy f (y:xss)

-- q10

myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: (Ord a) => [a] -> a
myMinimum xs = myMinimumBy compare xs
