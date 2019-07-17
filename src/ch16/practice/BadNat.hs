module BadNat where

type Nat f g a = f a -> g a

-- this'll work
maybeToList :: Nat Maybe [] a
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- this will also work if we tell it that 'a' is Num a => a
degenerateMtl :: Num a => Nat Maybe [] a
degenerateMtl Nothing = []
degenerateMtl (Just a) = [a+1]
