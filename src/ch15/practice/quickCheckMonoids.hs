module QuickCheckMonoids where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
asc (<>) a b c  = a <> (b <> c) == (a <> b) <> c

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)


-- testing left and right associativity
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools

type BullMappend =
  Bull -> Bull -> Bull -> Bool

-- fails on the left and right identity checks
main :: IO ()
main = do
  let ma  = monoidAssoc
      mli = monoidLeftIdentity
      mri = monoidRightIdentity
  quickCheck (ma  :: BullMappend) 
  quickCheck (mli :: Bull -> Bool) 
  quickCheck (mri :: Bull -> Bool) 
  
