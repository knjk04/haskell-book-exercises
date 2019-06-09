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

-- q1: filter DbDate alues and return a list of the UTCTime values inside them
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = undefined

-- q2: filter DbNumber values and return a list of the Integer values inside them
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = undefined

-- q3: get the most recent date
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = undefined

-- q4: sum all of the DbNumber values
sumDb :: [DatabaseItem] -> Integer
sumDb xs = undefined
-- sumDb xs = foldr (+) 0 xs

-- q5: calculate the average of the DbNumber values
avgDb :: [DatabaseItem] -> Double
avgDb = undefined
