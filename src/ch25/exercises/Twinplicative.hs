{-# LANGUAGE InstanceSigs #-}
module Twinplicative where

newtype Compose f g a =
  -- getCompose :: Compose f g a -> f (g a)
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


  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  -- (<*>) = undefined
  -- (Compose f) <*> (Compose f') = Compose $ f (getCompose f')
  -- (Compose f) <*> (Compose f') = Compose $ f . f'
  -- (Compose f) <*> (Compose f') = getCompose f f'
  -- (Compose f) <*> (Compose f') = Compose $ ((<*>) . (<*>)) f'
  -- (Compose f) <*> (Compose f') = Compose $ f f'

  -- (Compose f) <*> (Compose f') = Compose $ fmap f (Compose f')
