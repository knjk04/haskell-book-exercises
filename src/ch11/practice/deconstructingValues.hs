module DeconstructingValues where

newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

-- "FarmerType is a sum"
data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving Show

-- "Farmer is a plain ole proudct of Name, Acres and FarmerType"
data Farmer =
  Farmer Name Acres FarmerType

-- (could just do isDairyFarmer (Farmer _ _ farmerType) = farmerType == DairyFarmer if FarmerType
-- derived Eq)
isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer (Farmer _ _ _) = False

data FarmerRec =
  FarmerRec { name       :: Name
            , acres      :: Acres
            , farmerType :: FarmerType }
  deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
    DairyFarmer -> True
    _           -> False
