module MonadOfFunctions where

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

-- want one function that increments the values in the structure and returns the length of the structure
froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

-- or do the same thing by combining the two existing functions we have

-- bar only takes two arguments, but this version takes one argument. Both parts of the tuple will apply
-- to the same argument
barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

-- increment the values in the list like foo does
barPlus r = (foo r, length r)

-- more compact than above
frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

-- abstracted out so that it is not specific to foo and bar
fooBind :: (r -> a)
        -> (a -> r -> t)
        -> (r -> b)
fooBind m k = \r -> k (m r) r 
