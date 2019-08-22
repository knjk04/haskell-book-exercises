{-# LANGUAGE InstanceSigs #-}
module Twinplicative where

{-# LANGUAGE InstanceSigs #-}

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) =>
         Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

-- write the applicative instance
instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
  -- solution from https://github.com/Cake42/haskell-programming-from-first-principles/blob/master/25/Compose.hs
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) = undefined
