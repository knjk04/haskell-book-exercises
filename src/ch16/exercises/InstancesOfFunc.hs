module InstancesOfFunc where

import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-- 1)
newtype Identity a = Identity a

-- instance Functor (Identity a) where
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

-- 2)
data Pair a = Pair a a

-- instance Functor (Pair a) where
instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

-- 3)
data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

-- 4)
data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

-- 5)
data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

-- 6)
data Four a b c d = Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

-- 7)
data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)
