{-# LANGUAGE TypeFamilies, DefaultSignatures #-}

module AS.Types.Cell
  ( module AS.Types.Cell
  , module AS.Types.Locations
  , module AS.Types.CellProps
  , module AS.Types.Values
  ) where

import AS.Prelude
import Prelude()

import AS.ASJSON

import AS.Types.Locations
import AS.Types.RangeDescriptor
import AS.Types.CellProps
import AS.Types.Errors
import AS.Types.Formats
import AS.Types.Updates
import AS.Types.Values

import GHC.Generics
import Data.Aeson
import Data.List
import Data.SafeCopy
import qualified Data.Map as M 

import Data.Aeson.Types (Parser)
import Control.DeepSeq
import Control.Lens hiding ((.=))
import Control.Applicative (liftA2)
import Control.DeepSeq.Generics (genericRnf)

data ASLanguage = R | Python | OCaml | CPP | Java | SQL | Excel deriving (Show, Read, Eq, Data, Typeable, Generic)

data ASExpression = Expression { _expression :: String, _language :: ASLanguage } deriving (Show, Read, Eq, Data, Typeable, Generic)

data ASCell = Cell { _cellLocation :: ASIndex
                   , _cellExpression :: ASExpression
                   , _cellValue :: ASValue
                   , _cellProps :: ASCellProps
                   , _cellRangeKey :: Maybe RangeKey
                   , _cellDisplay :: Maybe String } 
                   deriving (Read, Show, Eq, Generic)
                   -- If the cell is not part of a range, cellRangeKey is Nothing; otherwise, it's the key to that range. 
                   -- In principle, cellRangeKey can be determined from the collection of all RangeKeys -- we can just find
                   -- the unique RangeKey that contains the location of the cell. However, this is relatively difficult to
                   -- implement efficiently (would probably involve storing the RangeKeys in a nontrivial structure in the 
                   -- database) and would be O(log # RangeKeys) rather than O(1), so we're just going to duplicate 
                   -- this information 

makeLenses ''ASExpression
makeLenses ''ASCell

-- NORM: never expand this type; always modify it using the records. (So we don't confuse 
-- before and after accidentally.)
type CellDiff = Diff ASCell 
type CellUpdate = Update ASCell ASReference

----------------------------------------------------------------------------------------------------------------------------------------------
-- Instances

instance HasKey ASCell where
  type KeyType ASCell = ASReference
  key = IndexRef . view cellLocation

asLensedToFromJSON ''ASExpression
asToFromJSON ''ASLanguage

-- -- Every time cells get updated, frontend gets passed a list of range descriptors, which it uses to 
-- -- determine whether the newly updated cells belong to a range or not; cellRangeKey doesn't need to get
-- -- exposed to frontend. 
-- instance ToJSON ASCell where
--   toJSON (Cell l e v ps _) = object ["cellLocation" .= l,
--                                      "cellExpression" .= e, 
--                                      "cellValue" .= v, 
--                                      "cellProps" .= ps]

asLensedToJSON ''ASCell
asToJSON ''CellDiff
asLensedToJSON ''CellUpdate

deriveSafeCopy 1 'base ''ASExpression
deriveSafeCopy 1 'base ''ASLanguage
deriveSafeCopy 1 'base ''ASCell

----------------------------------------------------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------------------------------------------------

isColocated :: ASCell -> ASCell -> Bool
isColocated c1 c2 = c1^.cellLocation == c2^.cellLocation

-- checks if a cell is "blank", in the sense that it has NoValue
isBlank :: ASCell -> Bool
isBlank = (== NoValue) . view cellValue

-- checks if a cell is actually "empty", in the sense that it has no props and no expression.
isEmptyCell :: ASCell -> Bool
isEmptyCell = liftA2 (&&) (isEmpty . view cellProps) (null . view (cellExpression.expression))

mergeCells :: [ASCell] -> [ASCell] -> [ASCell]
mergeCells c1 c2 = map snd $ M.toList $ M.union (toMap c1) (toMap c2)
  
-- needed for benchmarks as well
toMap :: [ASCell] -> M.Map ASIndex ASCell
toMap cs = M.fromList $ zip (map (view cellLocation) cs) cs

-- Returns a list of blank cells at the given locations. For now, the language doesn't matter, 
-- because blank cells sent to the frontend don't get their languages saved. 
blankCellAt :: ASIndex -> ASCell
blankCellAt l = Cell l (Expression "" Excel) NoValue emptyProps Nothing Nothing

blankCellsAt :: [ASIndex] -> [ASCell]
blankCellsAt = map blankCellAt

isMemberOfSpecifiedRange :: RangeKey -> ASCell -> Bool
isMemberOfSpecifiedRange key cell = (Just key == cell^.cellRangeKey)

---- partitions a set of cells into (cells belonging to one of the specified ranges, other cells)
partitionByRangeKey :: [ASCell] -> [RangeKey] -> ([ASCell], [ASCell])
partitionByRangeKey cells [] = ([], cells)
partitionByRangeKey cells keys = liftListTuple $ map (go cells) keys
  where go cs k = partition (isMemberOfSpecifiedRange k) cs
        liftListTuple t = (concatMap fst t, concatMap snd t)

getCellFormat :: ASCell -> Maybe Format
getCellFormat = fmap valFormat . getProp ValueFormatProp . view cellProps

getCellFormatType :: ASCell -> Maybe FormatType 
getCellFormatType = fmap (view formatType) . getCellFormat

execErrorToValueError :: ASExecError -> ASValue
execErrorToValueError e = ValueError (show e) "Exec error"

cellHasProp :: CellProp -> ASCell -> Bool
cellHasProp p = hasProp p .  view cellProps 

removeCellProp :: CellPropType -> ASCell -> ASCell
removeCellProp pt = cellProps %~ (removeProp pt)

setCellProp :: CellProp -> ASCell -> ASCell 
setCellProp cp  = cellProps %~ setProp cp

mapCellLocation :: [ASCell] -> [ASIndex]
mapCellLocation = map (view cellLocation)

-- #expert how to make a lens from this??
getFormattedVal :: ASCell -> Formatted ASValue 
getFormattedVal c = Formatted (c^.cellValue) (getCellFormat c)

setFormattedVal :: ASCell -> Formatted ASValue -> ASCell
setFormattedVal c fv = c & cellValue .~ (fv^.orig) 
                         & cellProps %~ maybe id (setProp . ValueFormat) (fv^.format)
