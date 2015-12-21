{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Eval
  ( module AS.Types.Eval
  , module AS.Types.Locations
  , module AS.Types.Errors
  ) where

import AS.Types.CellProps
import AS.Types.Locations
import AS.Types.Errors
import AS.Types.Cell
import AS.Types.Updates
import qualified Data.List as L

import GHC.Generics
import Data.Aeson hiding (Array)
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import Control.Monad.Trans.Either
import qualified Data.Map as M
import Data.Serialize (Serialize)


-- turning a spreadsheet range into dataframe etc...
-- only needed during at syntax and list decoupling
data RangeDescriptor = RangeDescriptor { descriptorKey :: RangeKey, expandingType :: ExpandingType, attrs :: JSON }
  deriving (Show, Read, Eq, Generic)

-- For internal use only. Represents a "cell" that takes up numerous cells (e.g., range(10)).
data FatCell = FatCell { expandedCells :: [ASCell], descriptor :: RangeDescriptor } deriving (Show, Read)
data CompositeCell = Single ASCell | Fat FatCell

instance FromJSON RangeDescriptor
instance ToJSON RangeDescriptor


type RListKey = String

type ValMap = M.Map ASIndex ASCell

-- This should be thought of as a mini spreadsheet used by eval as a cache (which can be updated)
data EvalContext = EvalContext { contextMap :: ValMap
                               , addedCells :: [ASCell]
                               , descriptorDiff :: DescriptorDiff }
                               deriving (Show, Read, Eq)

-- NORM: never expand this type. always modify and access it using the records.
type DescriptorDiff = Diff RangeDescriptor


emptyContext :: EvalContext
emptyContext = EvalContext M.empty [] emptyDiff

-- ephemeral types produced by eval 
-- that will expand in createListCells
type Array = [ASValue]
type Matrix = [Array] 
data Collection = A Array | M Matrix deriving (Show, Read, Eq, Generic)


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

data DescendantsSetting = ProperDescendants | DescendantsWithParent deriving (Show, Read, Eq)
data AncestrySetting = SetAncestry | DontSetAncestry deriving (Show, Read, Eq)

----------------------------------------------------------------------------------------------------------------------
-- Fat cells

--getListType :: ListKey -> String
--getListType key = last parts
--  where parts = splitBy keyPartDelimiter key
indexIsHead :: ASIndex -> RangeKey -> Bool
indexIsHead idx (RangeKey idx' _) = idx == idx'

rangeKeyToIndices :: RangeKey -> [ASIndex]
rangeKeyToIndices k = rangeToIndices range
  where range = Range (locSheetId . keyIndex $ k) (rangeRect k)

rangeRect :: RangeKey -> Rect
rangeRect (RangeKey idx dims) = (tl, br)
  where 
    Index _ (col, row) = idx
    tl = (col, row)
    br = (col + (width dims) - 1, row + (height dims) - 1)

rangeKeyToSheetId :: RangeKey -> ASSheetId
rangeKeyToSheetId = locSheetId . keyIndex

cellToRangeKey :: ASCell -> Maybe RangeKey
cellToRangeKey (Cell _ xp _ _ ) = case xp of 
  Coupled _ _ _ key -> Just key
  _ -> Nothing

isFatCellHead :: ASCell -> Bool 
isFatCellHead cell = case (cellToRangeKey cell) of 
  Just (RangeKey idx _) -> cellLocation cell == idx
  Nothing -> False

isCoupled :: ASCell -> Bool
isCoupled c = case (cellExpression c) of 
  Coupled _ _ _ _ -> True
  _ -> False

isEvaluable :: ASCell -> Bool
isEvaluable c = isFatCellHead c || (not $ isCoupled c)


----------------------------------------------------------------------------------------------------------------------
-- Instances

instance FromJSON CompositeValue
instance ToJSON CompositeValue

instance FromJSON ExpandingValue
instance ToJSON ExpandingValue

instance FromJSON Collection
instance ToJSON Collection

instance FromJSON JSONField
instance ToJSON JSONField

instance FromJSON JSONValue
instance ToJSON JSONValue

instance FromJSON DescriptorDiff
instance ToJSON DescriptorDiff

instance Serialize RangeDescriptor
instance Serialize JSONField 
instance Serialize JSONValue 
instance Serialize Collection 
instance Serialize DescriptorDiff
instance Serialize CompositeValue
instance Serialize ExpandingValue

-- memory region exposure instances for R value unboxing
instance NFData ASValue             where rnf = genericRnf
instance NFData CompositeValue      where rnf = genericRnf
instance NFData ExpandingValue      where rnf = genericRnf
instance NFData Collection          where rnf = genericRnf