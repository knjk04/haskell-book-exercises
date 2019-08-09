{-# LANGUAGE InstanceSigs #-}

module WriteStateYourself where

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

-- 1) implement the Functor instance for State
-- State :: (s -> (a, s)) -> State s a
instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  -- fmap f (Moi g) = Moi $ f g
  -- fmap f (Moi g) = Moi (f g, g)
  -- fmap f (Moi g) = Moi (f g, fmap f g)
  -- fmap f (Moi g) = Moi $ fmap f g
  -- fmap f (Moi g) = Moi g
  -- fmap f (Moi g) = Moi (f g)
  -- fmap f (Moi g) = Moi (f (fst g), snd g)
  -- fmap f (Moi g) = Moi (fst (f g), snd (f g))

  -- fmap f (Moi g) = Moi (f g)
  -- fmap f (Moi g) = Moi (g, f g)
  -- fmap f (Moi g) = Moi g

  -- fmap f (Moi g) = runMoi g 
  -- fmap f (Moi g) = (snd (runMoi g))
  fmap f (Moi g) = (snd g)
