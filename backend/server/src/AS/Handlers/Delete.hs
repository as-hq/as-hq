module AS.Handlers.Delete (handleDelete) where

import Data.List
import Control.Concurrent
import Control.Applicative
import Control.Lens
import qualified Data.Map as M

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

handleDelete :: MessageId -> ASUserClient -> ServerState -> ASRange -> IO ()
handleDelete mid uc state rng = do
  let conn = state^.dbConn
      inds = rangeToIndices rng
  blankedCells <- map removeBadFormats <$> getBlankedCellsAt conn inds -- need to know the formats at the old locations
  errOrUpdate <- runDispatchCycle state blankedCells DescendantsWithParent (userCommitSource uc) (modifyUpdateForDelete rng)
  broadcastErrOrUpdate mid state uc errOrUpdate

-- | Adds the range among the list of locations to delete, and remove all the update cells located within in range. 
modifyUpdateForDelete :: ASRange -> SheetUpdate -> SheetUpdate
modifyUpdateForDelete rng =
  cellUpdates %~ (& newVals %~ removeUpdateCellsInRange).(& oldKeys %~ addRangeRefs)
    where 
      rngs  = [rng] 
      cellContainedInRange r = rangeContainsIndex r . view cellLocation 
      cellContainedInRngs = or <$> sequence (map cellContainedInRange rngs)
      shouldKeepCell = not . liftA2 (&&) isEmptyCell cellContainedInRngs
      addRangeRefs = (map RangeRef rngs ++ )
      removeUpdateCellsInRange = filter shouldKeepCell
  -- #incomplete the type here should NOT be Selection. It should be a yet-to-be implemented type representing finite lists of cells
