{-# LANGUAGE TypeFamilies, DeriveGeneric, DefaultSignatures, TemplateHaskell #-}

module AS.Types.Cell
  ( module AS.Types.Cell
  , module AS.Types.Locations
  , module AS.Types.CellProps
  , module AS.Types.Values
  ) where

import AS.ASJSON

import AS.Types.Locations
import AS.Types.RangeDescriptor
import AS.Types.CellProps
import AS.Types.Errors
import AS.Types.Common
import AS.Types.Updates
import AS.Types.Values

import GHC.Generics
import Data.Aeson
import Data.List
import qualified Data.Map as M 

import Data.Serialize (Serialize)
import Data.Aeson.Types (Parser)
import Control.DeepSeq
import Control.Applicative (liftA2)
import Control.DeepSeq.Generics (genericRnf)

data ASLanguage = R | Python | OCaml | CPP | Java | SQL | Excel deriving (Show, Read, Eq, Generic)

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

data ASCell = Cell { cellLocation :: ASIndex
                   , cellExpression :: ASExpression
                   , cellValue :: ASValue
                   , cellProps :: ASCellProps } 
                   deriving (Read, Show, Eq, Generic)

-- NORM: never expand this type; always modify it using the records. (So we don't confuse 
-- before and after accidentally.)
type CellDiff = Diff ASCell 
type CellUpdate = Update ASCell ASReference

instance HasKey ASCell where
  type KeyType ASCell = ASReference
  key = IndexRef . cellLocation

instance ToJSON ASExpression where
  toJSON (Expression xp lang) = object ["expression" .= xp,
                                        "language" .= (show lang)]
  toJSON (Coupled xp lang dtype key) = object ["expression" .= xp,
                                               "language" .= (show lang),
                                               "expandingType" .= (show dtype),
                                               "rangeKey" .= key]
instance FromJSON ASExpression where
  parseJSON (Object v) = Expression <$> v .: "expression" <*> v .: "language"
instance Serialize ASExpression

asToFromJSON ''ASLanguage

asToJSON ''ASCell
asToJSON ''CellDiff
asToJSON ''CellUpdate


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
isEmptyCell = liftA2 (&&) (isEmpty . cellProps) (null . xpString . cellExpression)

mergeCells :: [ASCell] -> [ASCell] -> [ASCell]
mergeCells c1 c2 = map snd $ M.toList $ M.union (toMap c1) (toMap c2)
  
-- needed for benchmarks as well
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
