module ChapterExercises where

-- "write a Traversable instance for the datatype provided, filling in any required superclasses
-- Use QuickCheck to validate your instances

-- *** Identity ***

-- Write a Traversable instance for Identity
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a


-- *** CHECK THIS ONE WORKS ***

-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

-- *** Constant ***

-- newtype Constant a b =
--   Constant { getConstant :: a }

-- instance Functor (Constant a) where
--   -- fmap f (Constant a) = Constant $ f a 
--   fmap f (Constant a) = Constant $ a 

-- instance Foldable (Constant a) where
--   foldMap f (Constant a) = f a 

-- instance Traversable (Constant a) where
--   -- traverse f (Constant a) = f <$> a
--   traverse f (Constant a) = f <$> a

-- *** Maybe ***
data Optional a =
    Nada
  | Yep a

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada    = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

-- *** List ***
data List a =
    Nil
  | Cons a (List a)
  
instance Functor List where
  fmap _ Nil = Nil 
  fmap f (Cons a l) = Cons (f a) (fmap f l)

-- check this is right
instance Foldable List where
  foldMap f Nil = mempty
  foldMap f (Cons a l) = f a

instance Traversable List where
  traverse f Nil = pure Nil
  traverse f (Cons a l) = Cons <$> f a <*> (traverse f l)

-- *** List ***
data Three a b c =
  Three a b c

instance Functor (Three a b) where
  -- fmap f (Three a b c) = Three $ a b (f c)
  -- fmap f (Three _ _ c) = Three $ _ _ (f c)
  -- fmap f (Three _ _ c) = Three $ f c
  fmap f (Three a b c) = Three a b (f c)
