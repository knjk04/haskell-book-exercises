{-# LANGUAGE InstanceSigs #-}

module WriteStateYourself where

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

-- 1) implement the Functor instance for State
-- State :: (s -> (a, s)) -> State s a
-- solution from https://github.com/CarlosMChica/HaskellBook/blob/master/chapter23/WriteStateYourself.hs
instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> let (x, s') = g s
                               in (f x, s')

-- 2) write the Applicative instance for State
instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = ???

  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (Moi f) <*> (Moi g) =
    ???
