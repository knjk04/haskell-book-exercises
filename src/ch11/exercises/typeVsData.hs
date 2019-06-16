module TypeVsData where

data Price =
  Price Integer deriving (Eq, Show)

-- Q6
data Size =
    XS
  | Small 
  | Medium
  | Large
  | XL
  | XXL
    deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda 
  | Tata
    deriving (Eq, Show)

data Airline =
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)

-- sample data
myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir Medium

-- Q2 a)
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

-- Q2 b)
isPlane :: Vehicle -> Bool
isPlane (Plane _ _ ) = True
isPlane _            = False

-- Q2 c)
areCars :: [Vehicle] -> [Bool]
areCars xs = map isCar xs

-- Q3 and Q4: returns the manufacturer of a vehicle
getManu :: Vehicle -> Manufacturer
getManu (Car m _)   = m
getManu (Plane _ _) = undefined -- intentionally not returning the manufacturer since we do not
-- know it (airline may not have manufactured the plane)
