module Ch6Exercises where

import Data.List

----- * Eq Instances * -----

-- q1
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn n) (TisAn n') = n == n'
  -- (TisAn n) == (TisAn n') = n == n'

-- q2
data TwoIntegers =
  Two Integer Integer
  
instance Eq TwoIntegers where
  (==) (Two n m) (Two n' m') = n == n' && m == m'

-- q3
data StringOrInt =
    TisAnInt   Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt n) (TisAnInt n') = n == n'
  (==) (TisAString s) (TisAString s') = s == s'
  (==) _ _ = False

-- q4
data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair a' b') = a == a' && b == b'

-- q5  
data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

-- q6
data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False

-- q7
data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'
  (==) _ _ = False


----- * Match the types * -----

-- q1

i :: Num a => a
-- i :: a -- no because it isn't maximally polymorphic (1 is constrained by the type class Num)
i = 1

-- q2

f :: Float
-- f :: Num a => a -- No. It has to be constrained by the type class fractional (some Nums, such as
-- the integrals, are not
f = 1.0

-- q3

f3 :: Float
-- f3 :: Fractional a => a -- yes. Reason given in q2
f3 = 1.0

-- q4

f4 :: Float
-- f4 :: RealFrac a => a -- yes since RealFrac inherits from Fractional
f4 = 1.0

-- q5

freud :: a -> a
-- freud :: Ord a => a -> a yes
freud x = x

-- q6 

freud' :: a -> a  
-- freud' :: Int -> Int -- yes
freud' x = x

-- q7

myX = 1 :: Int

sigmund :: Int -> Int
-- sigmund x = myX -- yes
sigmund x = myX

-- q8

sigmund' :: Int -> Int
-- sigmund' :: Num a => a -> a  -- yes. Wrong. Answer is no because you can't go from more specific
-- to less specific
sigmund' x = myX

-- q9

jung :: Ord a => [a] -> a
-- jung :: [Int] -> Int -- yes
jung xs = head (sort xs)

-- q10

young :: [Char] -> Char
-- young :: Ord a => [a] -> a -- yes
young xs = head (sort xs)

-- q11

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a -- yes. Wrong
signifier xs = head (mySort xs)

----- * Type-Kwon-Do Two: Electric Typealoo * -----

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = aToB a == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB _ a = aToB a * 10


