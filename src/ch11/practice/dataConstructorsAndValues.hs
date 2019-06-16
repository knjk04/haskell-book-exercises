module DataConstructorsAndValues where

data PugType = PugData

-- Type variable a is not an argument to HuskyData
-- the type argument a is phanton (or "has not witness")
data HuskyType a = HuskyData

data DogueDeBordeaux doge = DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData 

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[Int]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

data Doggies a =
  Husky a
  | Mastiff a
  deriving (Eq, Show)
