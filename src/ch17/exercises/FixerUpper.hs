module FixerUpper where

-- Use (<$>) from Functor, and (<*>) and pure from Applicative
-- to fix the following pieces of broken code

-- 1)
-- q1 = const <$> Just "Hello" <*> "World" -- broken
q1 = const <$> Just "Hello" <*> pure "World" -- fixed

-- 2)
-- q2 = (,,,) Just 90 <*> Just 10 Just "Tierness" [1,2,3] -- broken
q2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3] -- fixed

