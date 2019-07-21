module ListApplicative where

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (f <$> l)

-- from https://github.com/mvaldesdeleon/haskell-book/blob/master/ch17/exercises/src/Exercises.hs
instance Applicative List where
  pure x           = Cons x Nil
  Nil <*> _        = Nil
  _ <*> Nil        = Nil
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
