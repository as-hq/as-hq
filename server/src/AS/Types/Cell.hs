{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

module AS.Types.Cell
  ( module AS.Types.Cell
  , module AS.Types.Locations
  , module AS.Types.CellProps
  ) where

import AS.Types.Locations
import AS.Types.CellProps
import AS.Types.Errors
import AS.Types.Common

import GHC.Generics
import Data.Aeson
import Data.List

import qualified Data.Map as M
import Data.Serialize (Serialize)
import Data.Aeson.Types (Parser)
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)

data ASLanguage = R | Python | OCaml | CPP | Java | SQL | Excel deriving (Show, Read, Eq, Generic)

data ExpandingType = List | RList | RDataFrame | NPArray | NPMatrix | PDataFrame | PSeries deriving (Show, Read, Eq, Generic)

data ASExpression =
    Expression { expression :: String, language :: ASLanguage }
  | Coupled { cExpression :: String, cLanguage :: ASLanguage, cType :: ExpandingType, cRangeKey :: RangeKey }
  deriving (Read, Show, Eq, Generic)

xpString :: ASExpression -> String
xpString (Expression xp _) = xp
xpString (Coupled xp _ _ _) = xp

xpLanguage :: ASExpression -> ASLanguage
xpLanguage (Expression _ lang) = lang
xpLanguage (Coupled _ lang _ _) = lang

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


data ASCell = Cell { cellLocation :: ASIndex
                   , cellExpression :: ASExpression
                   , cellValue :: ASValue
                   , cellProps :: ASCellProps } 
                   deriving (Read, Show, Eq, Generic)

-- range keys are used to access range descriptors, which relay metadata about a range of cells
-- e.g. for embedded lists and objects
data RangeKey = RangeKey { keyIndex :: ASIndex
                         , keyDimensions :: Dimensions } 
                         deriving (Show, Read, Eq, Generic)

instance ToJSON ASExpression where
  toJSON (Expression xp lang) = object ["expression" .= xp,
                                        "language" .= (show lang)]
  toJSON (Coupled xp lang dtype key) = object ["expression" .= xp,
                                               "language" .= (show lang),
                                               "expandingType" .= (show dtype),
                                               "rangeKey" .= key]
instance FromJSON ASExpression where
  parseJSON (Object v) = do
    dType <- (v .:? "expandingType") :: Parser (Maybe ExpandingType)
    case dType of 
      Just _ -> Coupled <$> v .: "expression"
                           <*> v .: "language"
                           <*> v .: "expandingType"
                           <*> v .: "rangeKey"
      Nothing -> Expression <$> v .: "expression" <*> v .: "language"

instance ToJSON ASCell
instance FromJSON ASCell

instance ToJSON ASLanguage
instance FromJSON ASLanguage

instance ToJSON ASValue
instance FromJSON ASValue

instance FromJSON ExpandingType
instance ToJSON ExpandingType

instance ToJSON RangeKey
instance FromJSON RangeKey

instance Serialize ASCell 
instance Serialize ASValue
instance Serialize ASExpression
instance Serialize RangeKey
instance Serialize ASLanguage
instance Serialize ExpandingType


----------------------------------------------------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------------------------------------------------

isColocated :: ASCell -> ASCell -> Bool
isColocated c1 c2 = (cellLocation c1) == (cellLocation c2)

-- checks if a cell is "blank", in the sense that it has NoValue
isBlank :: ASCell -> Bool
isBlank (Cell _ _ NoValue _) = True
isBlank _ = False

-- checks if a cell is actually "empty", in the sense that it has no props and no expression.
isEmptyCell :: ASCell -> Bool
isEmptyCell c = (null . underlyingProps $ cellProps c) && (null . xpString $ cellExpression c)

mergeCells :: [ASCell] -> [ASCell] -> [ASCell]
mergeCells c1 c2 = map snd $ M.toList $ M.union (toMap c1) (toMap c2)

toMap :: [ASCell] -> M.Map ASIndex ASCell
toMap cs = M.fromList $ zip (map cellLocation cs) cs

-- | Returns a list of blank cells at the given locations. For now, the language doesn't matter, 
-- because blank cells sent to the frontend don't get their languages saved. 
blankCellAt :: ASIndex -> ASCell
blankCellAt l = Cell l (Expression "" Excel) NoValue emptyProps

blankCellsAt :: [ASIndex] -> [ASCell]
blankCellsAt = map blankCellAt

isMemberOfSpecifiedRange :: RangeKey -> ASCell -> Bool
isMemberOfSpecifiedRange key cell = case (cellExpression cell) of 
  Coupled _ _ _ key' -> key == key'
  _ -> False

---- partitions a set of cells into (cells belonging to one of the specified ranges, other cells)
partitionByRangeKey :: [ASCell] -> [RangeKey] -> ([ASCell], [ASCell])
partitionByRangeKey cells [] = ([], cells)
partitionByRangeKey cells keys = liftListTuple $ map (go cells) keys
  where go cs k = partition (isMemberOfSpecifiedRange k) cs
        liftListTuple t = (concat $ map fst t, concat $ map snd t)

getCellFormatType :: ASCell -> Maybe FormatType
getCellFormatType (Cell _ _ _ props) = maybe Nothing (Just . formatType) $ getProp ValueFormatProp props

execErrorToValueError :: ASExecError -> ASValue
execErrorToValueError e = ValueError (show e) "Exec error"

removeCellProp :: CellPropType -> ASCell -> ASCell
removeCellProp pt (Cell l e v ps) = Cell l e v (removeProp pt ps)

setCellProp :: CellProp -> ASCell -> ASCell 
setCellProp cp (Cell l e v ps) = Cell l e v (setProp cp ps)
