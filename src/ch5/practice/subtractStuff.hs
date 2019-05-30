module SubtractStuff where

subtractStuff :: Integer -> Integer -> Integer
subtractStuff x y = x - y - 10

subtractOne = subtractStuff 1


-- manual currying and uncurrying

nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

-- (Haskell is curried by default)  

curriedFunction :: Integer -> Bool -> Integer
curriedFunction i b = i + (nonsense b)

uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i, b) = i + (nonsense b)
  
anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonNested :: Integer -> Bool -> Integer
anonNested = \i -> \b -> i + (nonsense b)
