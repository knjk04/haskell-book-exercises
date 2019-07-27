module IdentityFoldable where

data Identity a =
  Identity a

-- only obligated to write foldr or foldMap
instance Foldable Identity where
  foldr f z (Identity x) = f x z

  foldl f z (Identity x) = f z x

  foldMap f (Identity x) = f x
  

