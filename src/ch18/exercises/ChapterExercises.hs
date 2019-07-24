module ChapterExercises where

import Control.Monad (join)
import Control.Applicative

-- 1)

data Nope a =
  NopeDotJpg
  deriving Show

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure  _   = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

-- Only need (>>=) for a minimally complete Monad instance
-- (>>=) :: m a -> (a -> m b) -> m b
-- (>>) :: m a -> m b -> m b
-- return :: a -> m a 
instance Monad (Nope) where
  (>>=) _ _ = NopeDotJpg
  
-- 2)
data BahEither b a =
    PLeft a
  | PRight b

instance Functor (BahEither a) where
  fmap f (PLeft a)  = PLeft (f a)
  fmap f (PRight b) = PRight b

instance Applicative (BahEither a) where
  pure a = PLeft a
  (PLeft f) <*> (PLeft a) = PLeft (f a)
  (PRight f) <*> (PRight b) = PRight b
  _ <*> (PRight b) = PRight b

-- (>>=) :: m a -> (a -> m b) -> m b
instance Monad (BahEither a) where
  (>>=) (PLeft a) f = f a 
  (>>=) (PRight b) f = PRight b

-- 3) "Write a Monad instance for Identity"
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
instance Applicative Identity where
  pure a = Identity a
  (Identity f) <*> (Identity a) = Identity $ f a

instance Monad Identity where
  (Identity a) >>= f = f a

-- 4)
data List a =
    Nil
  | Cons a (List a)
  
-- fmap :: Functor f => (a -> f b) -> f a -> f b
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

-- Not sure if this actually works
instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f g) <*> xs = fmap f xs `append` (g <*> xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- need to write using concat' and fmap
flatMap :: (a -> List b)
        -> List a
        -> List b
flatMap f as = concat' $ f <$> as        

----- *** "Write the following functions using the methods provided by Monad and
----- Functor." Allowed to use identity and composition, but it must typecheck
----- with the types provided ***

-- 1) 
j :: Monad m => m (m a) -> m a
j m = join m

-- 2)
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f a = f <$> a

-- 3)
l2 :: Monad m
   => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = f <$> a <*> b

-- 4) need to use recursion
-- Broken :(
meh :: Monad m
    => [a] -> (a -> m b) -> m [b]
meh [] f = pure []
meh (x : xs) f = ((pure x) >>= f) >> (meh xs f)

-- 5) reuse meh
-- can't until I've fixed meh 
