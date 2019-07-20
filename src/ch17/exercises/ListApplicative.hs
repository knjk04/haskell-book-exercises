module ListApplicative where

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (List a) = List (f a)
  -- fmap f (Cons a (List a)) = List (f a)
