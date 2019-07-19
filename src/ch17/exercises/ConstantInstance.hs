module ConstantInstance where

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a 

instance Monoid a => Applicative (Constant a) where
  -- pure :: a -> f a
  pure a = Constant mempty
  -- (<*>) :: f (a -> b) -> f a -> f b
  (<*>) (Constant f) (Constant fa) = Constant (f <> fa)
