-- #needsrefactor selectedRanges should not be exposed, Currently it is because the correct type for Delete doesn't 
-- quite exist yet.
module AS.Types.Selection (Selection, activeIndex, indicesInSelection, selectedRanges) where

import AS.Prelude
import Prelude()

import AS.Types.Locations
import GHC.Generics
import Data.Aeson hiding (Success)
import Data.SafeCopy


-- will eventually need to include support for columns too
data Selection = Selection { activeIndex :: ASIndex, selectedRanges :: [ASRange] } deriving (Eq, Show, Read, Data, Typeable, Generic)

indicesInSelection :: Selection -> IO [ASIndex]
indicesInSelection = return . concatMap rangeToIndices . selectedRanges 

----------------------------------------------------------------------------------------------------------------------
-- Ancestors

-- #needsrefactor currently, only ranges are getting passed in. We should really be adding support for selections. 
instance ToJSON Selection
instance FromJSON Selection

deriveSafeCopy 1 'base ''Selection