module IntegralDivision where

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer
type Remainder = Integer

-- returns quotient and remainder (like divMod)
dividedBy :: Numerator -> Denominator -> (Quotient, Remainder)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise =
            go (n - d) d (count + 1)
