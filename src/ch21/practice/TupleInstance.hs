module TupleInstance where

-- import Prelude hiding (,)

instance Functor ((,) a) where
  fmap f (a, b) = (a, f b)

instance Applicative ((,) a) where
  pure x = (mempty, x)
  (u, f) <*> (v, x) =
    (u `mappend` v, f x)

-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- foldr :: Foldable f => (a -> b -> b) -> b -> t a -> b
instance Foldable ((,) a) where
  foldMap f (_, b)  = f b
  foldr f z (_, y)  = f z y
  
