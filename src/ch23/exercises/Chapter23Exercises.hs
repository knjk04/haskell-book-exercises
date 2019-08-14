{-# LANGUAGE InstanceSigs #-}

module Chapter23Exercises where

-- runState :: State s a -> s -> (a, s)
newtype State s a =
  State { runState :: s -> (a, s) }

-- 1) implement the Functor instance for State
-- State :: (s -> (a, s)) -> State s a
-- solution from https://github.com/CarlosMChica/HaskellBook/blob/master/chapter23/WriteStateYourself.hs
instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State $ \s -> let (x, s') = g s -- g :: s -> (a, s)
                               in (f x, s')

-- 2) write the Applicative instance for State
-- solution from https://github.com/CarlosMChica/HaskellBook/blob/master/chapter23/WriteStateYourself.hs
instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \s -> (a, s)

  (<*>) :: State s (a -> b)
        -> State s a
        -> State s b
  (State f) <*> (State g) =
    State $ \s -> let (f', s') = f s -- f' gets set to State s. f' :: (a -> b) -> State s b
                      (x, s'') = g s' -- x :: a; g :: s -> (a, s)
                  in (f' x, s')

-- 3) write the Monad instance for State
-- solution from https://github.com/CarlosMChica/HaskellBook/blob/master/chapter23/WriteStateYourself.hs
instance Monad (State s) where
  return = pure

  (>>=) :: State s a
        -> (a -> State s b)
        -> State s b
  (State f) >>= g =
    State $ \s -> let (x, s') = f s -- (x, s') :: (a, s)
                  in runState (g x) s'

-- 1) "Construct a State where the state is also the value you return"
get :: State s s
get = State $ \s -> (s, s)

-- 2) "Construct a State where the resulting state is the argument provided and the value is
-- defaulted to unit"
put :: s -> State s ()
put s = State $ \_ -> ((), s)

-- 3) "Run the State with s and get the state that results"
exec :: State s a -> s -> s
exec (State sa) s = snd $ runState (State sa) s
