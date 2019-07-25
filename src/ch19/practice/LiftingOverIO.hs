module LiftingOverIO where

import Data.Time.Clock

-- NominalDiffTime is a newtype of Pico and has a Num instance
-- (which is why the arithmetic works)
-- addUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
-- getCurrentTime :: IO ()
-- fmap :: (UTCTime -> UTCTime) -> IO UTCTime -> IO UTCTime
offsetCurrentTime :: NominalDiffTime
                  -> IO UTCTime
offsetCurrentTime offset =
  fmap (addUTCTime (offset * 24 * 3600)) $
    getCurrentTime
  
