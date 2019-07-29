module EitherInstance where

import Prelude hiding (Either, Left, Right)

data Either a b =
    Left a
  | Right b
  deriving (Eq, Ord, Show)

instance Functor (Either a) where
  fmap _ (Left a)  = Left a
  fmap f (Right b) = Right $ f b

instance Applicative (Either a) where
  pure             = Right 
  (Left a) <*> _   = Left a
  (Right f) <*> r  = f <$> r

-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
instance Foldable (Either a) where
  foldMap _ (Left _)  = mempty
  foldMap f (Right b) = f b

  foldr f z (Left y)   = z
  foldr f z (Right y)  = f y z
