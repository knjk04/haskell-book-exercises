module ListyInstances where

-- Need to run ghc -dynamic -I. --make ListyInstances.hs

import Data.Monoid
import Listy

instance Semigroup (Listy a) where
  (<>) (Listy l) (Listy l') =
    Listy $ mappend l l'

instance Monoid (Listy a) where
  mempty = Listy []

  
