module ChapterExercises where

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
  -- fmap f (Cons a l) = Cons (f a) l
