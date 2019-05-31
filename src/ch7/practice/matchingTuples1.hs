module TupleFunctions where

addUp2 :: Num a => (a, a) -> a
addUp2 (x, y) = x + y

addUp2Alt :: Num a => (a, a) -> a
addUp2Alt tup = fst tup + snd tup

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

third3 :: (a, b, c) -> c
third3 (_, _, c) = c

