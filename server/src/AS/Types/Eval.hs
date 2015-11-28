{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Eval
  ( module AS.Types.Eval
  , module AS.Types.Locations
  , module AS.Types.Errors
  ) where

import AS.Types.CellProps
import AS.Types.Locations
import AS.Types.Errors

import GHC.Generics
import Data.Aeson hiding (Array)
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import Control.Monad.Trans.Either
import qualified Data.Map as M

type RListKey = String

type ValMap = M.Map ASIndex CompositeValue
type FormattedValMap = M.Map ASIndex (Formatted CompositeValue)

data ExpandingType = List | RList | RDataFrame | NPArray | NPMatrix | PDataFrame | PSeries deriving (Show, Read, Eq, Generic)
-- [Dragme, matrix, array...] x [python, r, ocmal....]

-- ephemeral types produced by eval 
-- that will expand in createListCells
type Array = [ASValue]
type Matrix = [Array] 
data Collection = A Array | M Matrix deriving (Show, Read, Eq, Generic)

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

data ExpandingValue = 
    VList Collection
  | VRList [(RListKey, Array)]
  | VRDataFrame {rdfLabels :: Array, rdfIndices :: Array, rdfValues :: Matrix}
  | VNPArray Collection
  | VNPMatrix Matrix
  | VPDataFrame {dfLabels :: Array, dfIndices :: Array, dfData :: Matrix}
  | VPSeries {seriesIndices :: Array, seriesData :: Array}
  deriving (Show, Read, Eq, Generic)

data CompositeValue = Expanding ExpandingValue | CellValue ASValue deriving (Show, Read, Eq, Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Parsing

-- this type is used in parsing. the flow for parsing a "complex" type is
-- String -> JSON -> CompositeValue -> FatCell -> [ASValue]
-- 
-- also included in RangeDescriptor

type JSON = M.Map JSONKey JSONField 
type JSONKey = String 
data JSONField = JSONTree JSON | JSONLeaf JSONValue deriving (Show, Read, Eq, Generic)
data JSONValue = ListValue Collection | SimpleValue ASValue deriving (Show, Read, Eq, Generic)

type JSONPair = (String, String)

type EitherTExec = EitherT ASExecError IO

instance ToJSON ASValue
instance FromJSON ASValue

instance FromJSON CompositeValue
instance ToJSON CompositeValue

instance FromJSON ExpandingValue
instance ToJSON ExpandingValue

instance FromJSON Collection
instance ToJSON Collection

instance FromJSON JSONField
instance ToJSON JSONField

instance FromJSON ExpandingType
instance ToJSON ExpandingType

instance FromJSON JSONValue
instance ToJSON JSONValue

-- memory region exposure instances for R value unboxing
instance NFData ASValue             where rnf = genericRnf
instance NFData CompositeValue      where rnf = genericRnf
instance NFData ExpandingValue      where rnf = genericRnf
instance NFData Collection          where rnf = genericRnf