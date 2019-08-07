module Isomorphism where

type Iso a b = (a -> b, b -> a)

newtype Sum a = Sum { getSum :: a }

sumIsIsomorphicWithItsContents
  :: Iso a (Sum a)
sumIsIsomorphicWithItsContents =
  (Sum, getSum)
