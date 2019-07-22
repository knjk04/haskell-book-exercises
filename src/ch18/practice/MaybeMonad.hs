module MaybeMonad where

data Cow = Cow {
    name   :: String
  , age    :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0    = Just n
             | otherwise = Nothing

-- if the cow's name is Bess, it must weigh < 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
     then Nothing
     else Just c

-- bad version
mkSphericalCow :: String
               -> Int
               -> Int
               -> Maybe Cow
mkSphericalCow name age weight =
  case noEmpty name of
    Nothing -> Nothing

    Just name' ->
      case noNegative age of
        Nothing -> Nothing

        Just age' ->
          case noNegative weight of
            Nothing -> Nothing

            Just weight' ->
              weightCheck
              (Cow name' age' weight')
          

mkSphericalCow' :: String
               -> Int
               -> Int
               -> Maybe Cow
mkSphericalCow' name age weight = do
  name' <- noEmpty name
  age' <- noNegative age
  weight' <- noNegative weight
  weightCheck (Cow name' age' weight')

mkSphericalCow'' :: String
               -> Int
               -> Int
               -> Maybe Cow
mkSphericalCow'' name age weight = 
  noEmpty name >>=
  \name' ->
    noNegative age >>=
    \age' ->
      noNegative weight >>=
      \weight' ->
        weightCheck (Cow name' age' weight')

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i =
  if even i
  then Just (i + 1)
  else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)
