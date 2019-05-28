module EndOfCh3Exercises where

-- Q2

partA :: String -> String
partA x = x ++ "!"

partB :: String -> Char
partB x = x !! 4

partC :: String -> String
partC x = drop 9 x

-- Q3

thirdLetter :: String -> Char
thirdLetter x = x !! 2

-- Q4

letterIndex :: Int -> Char
letterIndex n = "Curry is awesome" !! n

-- Q5 (have to use drop and take and meant to only work for the defined string)

rvrs :: String
rvrs = drop 9 curry ++ (take 3 $ drop 5 curry) ++ " " ++ take 5 curry
  where curry = "Curry is awesome"
