module Listy where

newtype Listy a =
  Listy [a]
  deriving (Eq, Show)

instance Monoid (Listy a) where
  mempty = Listy []
