{-# LANGUAGE InstanceSigs #-}

module ReadingComprehension where

newtype Reader r a =
  Reader { runReader :: r -> a }

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  -- fmap f (Reader a) = undefined
  -- fmap f (Reader a) = Reader $ f a
  -- fmap f (Reader r) = Reader $ f a

  fmap f (Reader r) = Reader $ f a
    where a = runReader r

-- -- "To write the Applicative instance for Reader, we’ll use an
-- -- extension called InstanceSigs. It’s an extension we need
-- -- in order to assert a type for the type class methods. You
-- -- ordinarily cannot assert type signatures in instances. The
-- -- compiler already knows the type of the functions, so
-- -- it’s not usually necessary to assert the types in instances
-- -- anyway. We did this for the sake of clarity, to make the
-- -- Reader type explicit in our signatures."
-- instance Applicative (Reader r) where
--   pure :: a -> Reader r a
--   -- pure a = Reader $ ???
--   pure a = Reader id 

--   (<*>) :: Reader r (a -> b)
--         -> Reader r a
--         -> Reader r b
--   (Reader rab) <*> (Reader ra) =
--     -- Reader $ \r -> ???
--     Reader $ \r -> undefined
