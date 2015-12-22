module AS.Handlers.Delete (handleDelete) where

import AS.Types.Cell
import AS.Types.CellProps
import AS.Types.Network
import AS.Types.Messages
import AS.Types.Eval
import AS.Types.Commits
import AS.Types.Updates

import AS.Util
import AS.Dispatch.Core
import AS.DB.API

import AS.Config.Settings as CS
import AS.Reply

import Data.List

import Control.Concurrent

handleDelete :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleDelete uc state (PayloadR rng) = do
  conn <- dbConn <$> readMVar state
  blankedCells <- map removeBadFormats <$> getBlankedCellsAt conn (rangeToIndices rng) -- need to know the formats at the old locations
  errOrCommit <- runDispatchCycle state blankedCells DescendantsWithParent (userCommitSource uc) id
  let errOrDleteUpdate = fmap ((modifyUpdateForDelete rng) . sheetUpdateFromCommit) errOrCommit
  broadcastFiltered state uc $ makeReplyMessageFromErrOrUpdate errOrDleteUpdate

-- Deleting a cell keeps some of the formats but deletes others. This is the current list of formats
-- to remove upon deletion. 
badFormats :: [CellProp]
badFormats = [ValueFormat Date]

removeFormat :: CellProp -> ASCell -> ASCell
removeFormat p c = if (hasProp p (cellProps c)) then removeCellProp (propType p) c else c

removeFormats :: [CellProp] -> ASCell -> ASCell
removeFormats ps = foldl' (.) id (map removeFormat ps)

removeBadFormats :: ASCell -> ASCell
removeBadFormats = removeFormats badFormats

-- | Adds the range among the list of locations to delete, and remove all the update cells located within in range. 
modifyUpdateForDelete :: ASRange -> SheetUpdate -> SheetUpdate
modifyUpdateForDelete rng (SheetUpdate (Update cs locs) bs ds cfs) = SheetUpdate (Update cs' locs') bs ds cfs 
  where 
    locs' = (RangeRef rng):locs
    cs'   = filter (not . (rangeContainsIndex rng) . cellLocation) cs