module ChapterExercises where

import GHC.Arr

-- 1) not possible

-- 2)

data BoolAndSomethingElse a =
  False' a | True' a
  deriving Show

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)

-- 3)
data BoolAndMaybeSomethingElse a =
  Falsish | Truish a
  deriving Show

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish a) = Truish (f a)

-- 4) has the wrong type

newtype Mu f = InF { outF :: f (Mu f) }

-- instance Functor (Mu a) where
-- instance Functor (Mu)  where
  -- fmap f (InF a) = Mu (f a)

-- 5) wrong type

data D =
  D (Array Word Word) Int Int

-- instance Functor D where
--   fmap f D (Array w w') n n' = D 

----- *** Rearrange the arguments to the type constructor of the datatype so the Functor instance works ***

-- 1)
-- data Sum a b =
data Sum b a =
    First a
  | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

-- 2)


