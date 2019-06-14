module DbProcessing where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [DbDate (UTCTime
            (fromGregorian 1911 5 1)
   (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

-- ***** exercises: database processing *****

-- q1: filter DbDate values and return a list of the UTCTime values inside them
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate []            = []
filterDbDate (DbDate x:xs) = x : filterDbDate xs
filterDbDate (_:xs)        = filterDbDate xs

-- q2: filter DbNumber values and return a list of the Integer values inside them
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = [a | (DbNumber a) <- xs]

-- q3: get the most recent date
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = maximum $ filterDbDate xs

-- q4: sum all of the DbNumber values
sumDb :: [DatabaseItem] -> Integer
sumDb xs = sum $ filterDbNumber xs

-- q5: calculate the average of the DbNumber values
avgDb :: [DatabaseItem] -> Double
avgDb xs = (fromIntegral $ sumDb xs) / (fromIntegral $ length $ filterDbNumber xs)
