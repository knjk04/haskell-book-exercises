module OptionalMonoid where

import Data.Monoid

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) (Only x) Nada     = Only x
  (<>) Nada (Only y)     = Only y
  (<>) Nada Nada         = Nada
  (<>) (Only x) (Only y) = Only (x <> y)

instance Monoid a => Monoid (Optional a) where
  -- mempty a = Only a
  mempty  = Nada
  mappend = (<>)
  -- mappend (Only x) Nada     = Only x
  -- mappend Nada (Only y)     = Only y
  -- mappend (Only x) (Only y) = Only (x <> y)
  -- mappend Nada Nada         = Nada
