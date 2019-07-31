module ANewBeginning where

import Control.Applicative

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop
