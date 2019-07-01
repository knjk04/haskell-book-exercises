module Bools where

data Bool' a =
    False'
  | True'
  deriving (Eq, Show)

-- conjunction
instance Semigroup (Bool' a) where
  (<>) False' _    = False'
  (<>) _ False'    = False'
  (<>) True' True' = True'

instance Monoid (Bool' a) where
  mempty = True'
