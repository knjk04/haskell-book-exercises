{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- the language extension allows for Goats to reuse the Int instance

module Newtype where

-- newtype Cows =
--   Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)

-- "under the hood, Goats is still an Int", but by defining it as a newtype,
-- a new custom instance can be defined
-- instance TooMany Goats where
  -- tooMany (Goats n) = n > 43
  -- tooMany (Goats n) = (tooMany n)
