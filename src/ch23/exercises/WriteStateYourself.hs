{-# LANGUAGE InstanceSigs #-}

module WriteStateYourself where

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

-- 1) implement the Functor instance for State
-- State :: (s -> (a, s)) -> State s a
-- solution from https://github.com/CarlosMChica/HaskellBook/blob/master/chapter23/WriteStateYourself.hs
instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> let (x, s') = g s -- g :: s -> (a, s)
                               in (f x, s')

-- 2) write the Applicative instance for State
-- solution from https://github.com/CarlosMChica/HaskellBook/blob/master/chapter23/WriteStateYourself.hs
instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (Moi f) <*> (Moi g) =
    Moi $ \s -> let (f', s') = f s -- f' gets set to Moi s. f' :: (a -> b) -> Moi s b
                    (x, s'') = g s' -- x :: a; g :: s -> (a, s)
                in (f' x, s')

-- 3) write the Monad instance for State
-- solution from https://github.com/CarlosMChica/HaskellBook/blob/master/chapter23/WriteStateYourself.hs
instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (Moi f) >>= g =
    Moi $ \s -> let (x, s') = f s -- (x, s') :: (a, s)
                in runMoi (g x) s'
