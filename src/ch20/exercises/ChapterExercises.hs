module ChapterExercises where

-- "Write Foldable instances for the following datatypes"

-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m

-- 1)
data Constant a b =
  Constant b
  deriving Show

-- only need foldr or foldMap a minimally complete instance

-- Prelude>   foldMap (+2) (Constant 4) :: Sum Int
instance Foldable (Constant a) where
  -- foldMap f (Constant b) = f b
  foldMap f (Constant _) = mempty

-- 2)
data Two a b =
  Two a b
  deriving Show

-- Prelude> foldMap (+2) (Two 3 4) :: Sum Int
instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

-- 3)
data Three a b c =
  Three a b c
  deriving Show
  
-- Prelude> foldMap (+2) (Three 3 4 5) :: Sum Int
instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

-- 4)
data Three' a b =
  Three' a b b
  deriving Show
  
-- Prelude> foldMap (+2) (Three' 3 4) :: Sum Int
instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = (f b) <> (f b')

-- 5)
data Four' a b =
  Four' a b b b
  deriving Show

-- Prelude> foldMap (+2) (Four' 3 4 5 6) :: Sum Int

instance Foldable (Four' a) where
  foldMap f (Four' _ b b' b'') = f b <> f b' <> f b''

-- "write a filter function for Foldable types using foldMap"
filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f ta = undefined  
-- filterF f ta = foldMap f ta  
