module ZipListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype ZipList' a =
  ZipList' [a]
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take 3000 l
          ys' = let (ZipList' l) = ys
                in take 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

-- doesn't work
instance Applicative ZipList' where
  -- pure = undefined
  pure a = ZipList' [a]
  -- (<*>) = undefined
  -- ZipList' f <*> ZipList' xs = ZipList' <$> f <*> xs
  ZipList' f <*> ZipList' xs = ZipList' $ f <*> xs
  -- ZipList' fs <*> ZipList' xs = ZipList' $ concat [f <$> xs | f <- fs]
  -- ZipList' fs <*> ZipList' xs = ZipList' $ concat [f x | f <- fs, x <- xs]
  -- ZipList' fs <*> ZipList' xs = ZipList' $ zip fs xs

  -- ZipList' f <*> ZipList' xs = ZipList' $ f <*> xs

-- applyZipList :: ZipList' [(a -> b)] -> ZipList' [a] -> ZipList' [a]
-- applyZipList (ZipList' []) xs = xs
-- applyZipList _ (ZipList' []) = ZipList' []
-- applyZipList (ZipList' (f : fs)) (ZipList' (x : xs)) =
--   ZipList' $ f x : applyZipList (ZipList' fs) (ZipList' xs)
