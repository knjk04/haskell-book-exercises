module IdentityInstance where

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  -- pure :: a -> f a
  pure x = Identity x
  -- (<*>) :: f (a -> b) -> f a -> f b
  (<*>) (Identity f) (Identity fa) = Identity (f fa)
