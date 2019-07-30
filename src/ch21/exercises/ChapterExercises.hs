{-# LANGUAGE FlexibleContexts #-}

module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers

-- "write a Traversable instance for the datatype provided, filling in any required superclasses
-- Use QuickCheck to validate your instances"

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
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

-- *** Pair ***
data Pair a b =
  Pair a b

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

-- *** Big ***
-- "when you have more than value of type b, you'll want to use Monoid and Applicative for the
-- Foldable and Traversable instances"
data Big a b =
  Big a b b

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b)

instance Foldable (Big a) where
  foldMap f (Big _ b b') = f b <> f b'

instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'

-- *** S ***

data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a )
        => Arbitrary (S n a) where
  arbitrary =
    S <$> arbitrary <*> arbitrary

-- instance ( Applicative n
--          , Testable (n Property)
--          , Eq a
--          , Eq (n a)
--          , EqProp a)
--         => EqProp (S n a) where
--   (=-=) = eq

-- instance Traversable n => Traversable (S n) where
--   traverse = undefined

-- main =
--   sample' (arbitrary :: Gen (S [] Int))
