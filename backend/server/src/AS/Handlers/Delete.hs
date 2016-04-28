module AS.Handlers.Delete (handleDelete) where

import Data.List
import qualified Data.Set as S
import Control.Concurrent
import Control.Applicative
import Control.Lens
import qualified Data.Map as M

import AS.Prelude
import AS.Types.Cell
import AS.Types.CellProps
import AS.Types.Commits
import AS.Types.Eval
import AS.Types.Formats
import AS.Types.Messages
import AS.Types.Network
import AS.Types.Selection
import AS.Types.Updates

import AS.Util
import AS.Dispatch.Core
import AS.DB.API
import AS.Config.Settings as CS
import AS.Reply

-- Deleting a cell keeps some of the props but deletes others. This is the current predicate for formats
-- to keep upon deletion. 
shouldKeepPropAfterDelete :: CellProp -> Bool
shouldKeepPropAfterDelete (ValueFormat (Format Date _)) = False
shouldKeepPropAfterDelete (ImageData _ _ _ _) = False
shouldKeepPropAfterDelete _ = True

removeBadFormats :: ASCell -> ASCell
removeBadFormats = cellProps %~ filterProps shouldKeepPropAfterDelete

handleDelete :: MessageContext -> ASRange -> IO ()
handleDelete msgctx rng = do
  let inds = finiteRangeToIndices rng
  blankedCells <- map removeBadFormats <$> getBlankedCellsAt (msgctx^.dbConnection) inds -- need to know the formats at the old locations
  errOrUpdate <- runDispatchCycle msgctx blankedCells DescendantsWithParent (modifyUpdateForDelete rng)
  broadcastErrOrUpdate msgctx errOrUpdate

-- | Adds the range among the list of locations to delete, and remove all the update cells located within in range. 
modifyUpdateForDelete :: ASRange -> SheetUpdate -> SheetUpdate
modifyUpdateForDelete rng =
  cellUpdates %~ (& newValsSet %~ removeUpdateCellsInRange) . (& oldKeysSet %~ addRangeRefs)
    where 
      rngs  = S.singleton rng
      cellContainedInRange r = rangeContainsIndex r . view cellLocation 
      cellContainedInRngs = or <$> sequence (map cellContainedInRange (S.toList rngs))
      shouldKeepCell = not . liftA2 (&&) isEmptyCell cellContainedInRngs
      addRangeRefs = (S.union (S.map RangeRef rngs))
      removeUpdateCellsInRange = S.filter shouldKeepCell
  -- #incomplete the type here should NOT be Selection. It should be a yet-to-be implemented type representing finite lists of cells
