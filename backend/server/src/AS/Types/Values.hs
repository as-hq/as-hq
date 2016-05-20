{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module AS.Types.Values where 

import AS.Prelude
import AS.ASJSON

import AS.Types.Locations (Dimensions(..), int)
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import Data.List.Split (chunksOf)
import qualified Data.List as L
import qualified Data.Map as M
import Data.SafeCopy

-- exactly the values that can be contained in a single cell
data ASValue =
    NoValue
  | ValueNaN  
  | ValueInf 
  | ValueS String
  | ValueI Integer
  | ValueD Double
  | ValueB Bool
  | ValueImage { imagePath :: String }
  | ValueError { errorMsg :: String, errorType :: String }
  | ValueSerialized { serializedValue :: String, displayName :: String  }
  deriving (Show, Read, Eq, Generic)

type Array = [ASValue]
type Matrix = [Array] 
data Collection = A Array | M Matrix deriving (Show, Read, Eq, Generic)
type RListKey = String

data ExpandingValue = 
    VList Collection
  | VRList [(RListKey, Array)]
  | VRDataFrame {rdfLabels :: Array, rdfIndices :: Array, rdfValues :: Matrix}
  | VNPArray Collection
  | VNPMatrix Matrix
  | VPDataFrame {dfLabels :: Array, dfIndices :: Array, dfData :: Matrix}
  | VPSeries {seriesIndices :: Array, seriesData :: Array}
  deriving (Show, Read, Eq, Generic)

-- Represents all the types that could possibly arise from an evaluation. 
data CompositeValue = Expanding ExpandingValue | CellValue ASValue deriving (Show, Read, Eq, Generic)

----------------------------------------------------------------------------------------------------------------------
-- Instances

asToFromJSON ''ASValue
asToFromJSON ''Collection
asToFromJSON ''ExpandingValue
asToFromJSON ''CompositeValue

deriveSafeCopy 1 'base ''ASValue
deriveSafeCopy 1 'base ''Collection
deriveSafeCopy 1 'base ''ExpandingValue
deriveSafeCopy 1 'base ''CompositeValue

-- memory region exposure instances for R value unboxing
instance NFData ASValue             where rnf = genericRnf
instance NFData Collection          where rnf = genericRnf
instance NFData ExpandingValue      where rnf = genericRnf
instance NFData CompositeValue      where rnf = genericRnf

----------------------------------------------------------------------------------------------------------------------------------------------
-- Helpers

-- #RoomForImprovement: timchu. Do not use ^.int?
reshapeList :: [a] -> Dimensions -> [[a]]
reshapeList xs dims = chunksOf ((width dims)^.int) xs

prependColumn :: Array -> Matrix -> Matrix
prependColumn arr mat = map (\(x,xs) -> x:xs) $ zip arr mat

-- transposes non-rectangular matrices by filling in gaps with NoValue
transpose' :: [[ASValue]] -> [[ASValue]]
transpose' vals = L.transpose matrixified
  where
    width       = maximum $ map length vals
    matrixified = map (\row -> take width $ row ++ (repeat NoValue)) vals