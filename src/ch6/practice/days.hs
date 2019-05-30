module Days where

data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun
               deriving (Show)
-- deriving (Ord, Show)

-- "day of week and numerical day of month"
data Date =
  Date DayOfWeek Int deriving Show

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _     = False

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') = 
     weekday == weekday'
   && dayOfMonth == dayOfMonth'


 -- Ord instances

instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _   = GT
  compare _ Fri   = LT
  compare _ _     = EQ
