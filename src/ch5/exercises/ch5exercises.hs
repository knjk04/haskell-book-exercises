module Ch5Exercises where

----- * Parametricity exercises *

q2partOne :: a -> a -> a
q2partOne a b = a

q2partTwo :: a -> a -> a
q2partTwo a b = b

q3 :: a -> b -> b
q3 a b = b

----- * Given a type, write the function *

-- Q1) "only one function definition that typechecks and doesn't go into an infinite loop"
i :: a -> a
i a = a

-- Q2) only one implementation
c :: a -> b -> a
c a _ = a

-- Q3) only one implementation
c'' :: b -> a -> b
c'' b _ = b

-- Q4) only one implementation
c' :: a -> b -> b
c' _ b = b


-- Q5) multiple possible implementations

r :: [a] -> [a]
r xs = tail xs

r2 :: [a] -> [a]
r2 xs = drop 2 xs

r3 :: [a] -> [a]
r3 xs = reverse xs

-- Q6) only one implementation
co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC (aToB a)
  
-- Q7) only one implementation
a :: (a -> b) -> a -> b
a aToB a = aToB a

-- Q8) only one implementation
a' :: (a -> b) -> a -> b
a' aToB a = aToB a

----- * Type-Kwon-Do * -----

-- q1
f :: Int -> String
f = undefined -- not meant to add an implementation

g :: String -> Char
g = undefined -- not meant to add an implementation

h :: Int -> Char
h n = g $ f $ n

-- q2
data A
data B
data C

q :: A -> B
q = undefined -- not meant to add an implementation

w :: B -> C
w = undefined -- not meant to add an implementation

e :: A -> C
e a = w $ q $ a

-- q3
data X
data Y
data Z

xz :: X -> Z
xz = undefined -- not meant to add an implementation

yz :: Y -> Z
yz = undefined -- not meant to add an implementation

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

-- q4 
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xToY yToWz x = fst $ yToWz (xToY x)
