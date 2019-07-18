{-# LANGUAGE FlexibleInstances #-}

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
-- data Company a b c =
data Company a c b =
    DeepBlue a c
  | Something b
  deriving Show

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3)
-- data More a b =
data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

----- *** Write functor instances for the the following datatypes *** -----

-- 1)
data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving Show

instance Functor (Quant a) where
  fmap f (Desk a)  = Desk a 
  fmap f (Bloor b) = Bloor (f b)
  fmap _ Finance   = Finance

-- 2)
data K a b =
  K a
  deriving Show

instance Functor (K a) where
  fmap _ (K a) = K a

-- 3)

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K' a b =
  K' a
  deriving Show

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip $ K' (f a)

-- 4)
data EvilGoateeConst a b =
  GoatyConst b
  deriving Show

instance Functor (EvilGoateeConst b) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 5)
data LiftItOut f a =
  LiftItOut (f a)
  deriving Show

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- 6)
data Parappa f g a =
  DaWrappa (f a) (g a)
  deriving Show

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- 7)
data IgnoreOne f g a b =
  IgnoreSomething (f a) (g b)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething fa gb) = IgnoreSomething fa (fmap f gb)

-- 8)
data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- 9) need to use recursion
data List a =
    Nil
  | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a a') = Cons (f a) (fmap f a')

-- 10)
-- "a veritable hydra of goats"
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

-- 11)
data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print a b) = Print a (f b)
  fmap f (Read g) = Read (fmap f g)
