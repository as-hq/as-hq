module AS.Handlers.Delete (handleDelete) where

import AS.Types.Cell
import AS.Types.CellProps
import AS.Types.Network
import AS.Types.Messages
import AS.Types.Eval
import AS.Types.Commits
import AS.Types.Selection
import AS.Types.Updates

import AS.Util
import AS.Dispatch.Core
import AS.DB.API

import AS.Config.Settings as CS
import AS.Reply

import Data.List

import Control.Concurrent
import Control.Applicative
import Control.Lens

handleDelete :: ASUserClient -> MVar ServerState -> ASRange -> IO ()
handleDelete uc state rng = do
  conn <- dbConn <$> readMVar state
  let inds = rangeToIndices rng
  blankedCells <- map removeBadFormats <$> getBlankedCellsAt conn inds -- need to know the formats at the old locations
  errOrUpdate <- runDispatchCycle state blankedCells DescendantsWithParent (userCommitSource uc) (modifyUpdateForDelete rng)
  broadcastErrOrUpdate state uc errOrUpdate

-- Deleting a cell keeps some of the formats but deletes others. This is the current list of formats
-- to remove upon deletion. 
badFormats :: [CellProp]
badFormats = [ValueFormat Date]

removeFormat :: CellProp -> ASCell -> ASCell
removeFormat p c = if cellHasProp p c then removeCellProp (propType p) c else c

removeFormats :: [CellProp] -> ASCell -> ASCell
removeFormats ps = foldl' (.) id (map removeFormat ps)

removeBadFormats :: ASCell -> ASCell
removeBadFormats = removeFormats badFormats

-- | Adds the range among the list of locations to delete, and remove all the update cells located within in range. 
-- #lens
modifyUpdateForDelete :: ASRange -> SheetUpdate -> SheetUpdate
modifyUpdateForDelete rng (SheetUpdate (Update cs locs) bs ds cfs) = SheetUpdate (Update cs' locs') bs ds cfs 
  where 
    rngs  = [rng] 
    locs' = (map RangeRef rngs) ++ locs
    cellContainedInRange r = rangeContainsIndex r . view cellLocation 
    cellContainedInRngs = or <$> sequence (map cellContainedInRange rngs)
    cs'   = filter (not . liftA2 (&&) isEmptyCell cellContainedInRngs) cs
-- #incomplete the type here should NOT be Selection. It should be a yet-to-be implemented type representing finite lists of cells