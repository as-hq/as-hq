{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Selection (Selection, activeIndex, indicesInSelection) where

import AS.Types.Locations
import GHC.Generics
import Data.Aeson hiding (Success)
import Data.Serialize (Serialize)


-- will eventually need to include support for columns too
data Selection = Selection { activeIndex :: ASIndex, selectedRanges :: [ASRange] } deriving (Eq, Show, Read, Generic)

indicesInSelection :: Selection -> [ASIndex]
indicesInSelection = concatMap rangeToIndices . selectedRanges 

instance FromJSON Selection
instance ToJSON Selection
instance Serialize Selection