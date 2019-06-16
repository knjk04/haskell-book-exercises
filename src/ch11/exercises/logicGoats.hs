{-# LANGUAGE FlexibleInstances #-}

module LogicGoats where

class TooMany a where
  tooMany :: a -> Bool

-- 1) write an instance for (Int, String)
instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

-- 2) make a TooMany instance for (Int, Int) where you sum the values together
-- and then check if there are too many goats
instance TooMany (Int, Int) where
  tooMany (m, n) = (m + n) > 42

-- 3) a TooMany instance for (Num a, TooMany a) => (a,a)
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, a) = tooMany (n + a)
  -- tooMany (n, a) = n > 42 && (tooMany a)
