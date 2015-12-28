{-# LANGUAGE DeriveGeneric #-}

-- #needsrefactor selectedRanges should not be exposed, Currently it is because the correct type for Delete doesn't 
-- quite exist yet.
module AS.Types.Selection (Selection, activeIndex, indicesInSelection, selectedRanges) where

import AS.Types.Locations
import GHC.Generics
import Data.Aeson hiding (Success)
import Data.Serialize (Serialize)


-- will eventually need to include support for columns too
data Selection = Selection { activeIndex :: ASIndex, selectedRanges :: [ASRange] } deriving (Eq, Show, Read, Generic)

indicesInSelection :: Selection -> IO [ASIndex]
indicesInSelection = return . concatMap rangeToIndices . selectedRanges 

instance FromJSON Selection
instance ToJSON Selection
instance Serialize Selection