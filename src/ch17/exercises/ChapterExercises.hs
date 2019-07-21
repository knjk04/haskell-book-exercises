module ChapterExercises where

import Control.Applicative (liftA3)

-- 1) 
data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

-- 2)  
data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure a = Two mempty a
  (Two _ g) <*> (Two a b) = Two a (g b)

  -- 3)
data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure a = Three mempty mempty a
  (Three _ _ h) <*> (Three a b c) = Three a b (h c)

-- 4)
data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  pure a = Three' mempty a a
  (Three' _ g h) <*> (Three' a b c) = Three' a (g b) (h c)

-- 5)
data Four a b c d = Four a b c d deriving Show

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure a = Four mempty mempty mempty a
  (Four _ _ _ i) <*> (Four a b c d) = Four a b c (i d)

-- 6)
data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' $ f b

instance Monoid a => Applicative (Four' a)  where
 pure a = Four' mempty mempty mempty a
 (Four' _ _ _ i) <*> (Four' a b c d) = Four' a b c (i d)


----- ***** Combinations *****
-- write a function to generate the possible combinations of three input lists from Control.Applicative

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

-- make 3-tuples of all possible stop-vowel-stop combinations
-- Prelude> combos stops vowels stops
combos :: [a] -> [b] -> [c] -> [(a, b, c)]
-- combos xs ys zs = (,,) <$> xs <*> ys <*> zs
combos xs ys zs = liftA3 (,,) xs ys zs
