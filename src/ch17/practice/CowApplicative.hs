module CowApplicative where

import Control.Applicative -- for liftA3

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

cowFromString :: String
              -> Int
              -> Int
              -> Maybe Cow
cowFromString name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing

    Just name'' ->
      case noNegative age' of
        Nothing -> Nothing

        Just age'' ->
          case noNegative weight' of
            Nothing -> Nothing

            Just weight'' ->
              Just (Cow name'' age'' weight'')
              
cowFromString' :: String
              -> Int
              -> Int
              -> Maybe Cow
cowFromString' name' age' weight' =
  Cow <$> noEmpty name'
      <*> noNegative age'
      <*> noNegative weight'

cowFromString'' :: String
              -> Int
              -> Int
              -> Maybe Cow
cowFromString'' name' age' weight' =
  liftA3 Cow (noEmpty name')
             (noNegative age')
             (noNegative weight')
