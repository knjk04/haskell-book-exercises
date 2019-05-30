{-# LANGUAGE NoMonomorphismRestriction #-}
-- prevents top-level declarations defaulting to having a concrete type if there is one 

module DetermineTheType where

----- * q1 *

-- Value returned: 54 (Correct)
-- Type of result: Num a => a  (Correct)
partA = (* 9) 6

-- Value returned: (0, "doge") (Correct)
-- Type of result: Num a => (a, [Char]) (Correct)
partB = head [(0, "doge"), (1, "kitteh")]

-- Value returned: (0, "doge") (Correct)
-- Type of result: (Integer, [Char]) (Correct)
partC = head [(0 :: Integer, "doge"), (1, "kitteh")]  

-- Value returned: False (Correct)
-- Type of result: Bool (Correct)
partD = if False then True else False  

-- Value returned: 5 (Correct)
-- Type of result: Bool (Correct)
partE = length [1, 2, 3, 4, 5]

-- Value returned: False (Correct)
-- Type of result: Bool (Correct)
partF = (length [1, 2, 3, 4]) > (length "TACOCAT")
