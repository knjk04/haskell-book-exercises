module EndOfChapter4Exercises where

-- q8

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- q9 (have to use if-then-else)

myAbs :: Integer -> Integer
myAbs n = if (n < 0) then -n else n

-- q10

f :: (a, b) -> (c, d) -> ((b,d), (a,c))
f (a, b) (c, d) = ((b,d), (a,c))

-- Correcting syntax  

-- q1

x = (+)

-- renamed to 'f2' from 'f' to avoid name clash with the function 'f' above
f2 xs = x w 1 
  where w = length xs


-- q2

id' x = x

-- q3

-- renamed to 'f3' from 'f' to avoid name clash with the function 'f' above
f3 (a, b) = a
  

  

