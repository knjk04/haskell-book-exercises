module TransformingTheUnappliedTypeArgument where

data Two a b =
  Two a b
  deriving (Eq, Show)

data Or a b =
    First a
  | Second b
  deriving (Eq, Show)

-- can only map the inner argument because a is a part of the structure
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)
