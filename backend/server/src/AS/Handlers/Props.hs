module AS.Handlers.Props (handleToggleProp, handleSetProp, handleChangeDecimalPrecision) where

import AS.Prelude
import Prelude()
import AS.Config.Constants

import AS.Types.Cell
import AS.Types.Commits
import AS.Types.Eval
import AS.Types.Formats
import AS.Types.Messages
import AS.Types.Network
import AS.Types.User

import AS.Prelude 
import AS.DB.API
import AS.Dispatch.Core as DP
import AS.Daemon as DM
import AS.Reply
import AS.Util

import Data.List
import Control.Applicative ((<*>))
import Control.Concurrent
import Control.Lens
import qualified Data.Map as M
import Database.Redis (Connection)

-- | Used only for flag props. 
handleToggleProp :: MessageId -> ASUserClient -> ServerState -> CellProp -> ASRange -> IO ()
handleToggleProp mid uc state p rng = do
  let locs = rangeToIndices rng
      pt   =  propType p
      conn = state^.dbConn
  cells <- getPossiblyBlankCells conn locs
  let (cellsWithProp, cellsWithoutProp) = partition (hasPropType pt . view cellProps) cells
  -- if there's a single prop present in the range, remove this prop from all the cells; 
  -- otherwise set the prop in all the cells. 
  if (null cellsWithoutProp)
    then do 
      let cells' = map (cellProps %~ removeProp pt) cellsWithProp
          (emptyCells, nonEmptyCells) = partition isEmptyCell cells'
      setCells conn nonEmptyCells
      deleteLocs conn $ mapCellLocation emptyCells
      mapM_ (removePropEndware state p) nonEmptyCells
      sendSheetUpdate mid uc $ sheetUpdateFromCells cells'
    else do
      let cells' = map (cellProps %~ setProp p) cellsWithoutProp
      setCells conn cells'
      mapM_ (setPropEndware state p) cells'
      sendSheetUpdate mid uc $ sheetUpdateFromCells cells'
    -- don't HAVE to send back the entire cells, but that's an optimization for a later time. 
    -- Said toad. (Alex 11/7)

setPropEndware :: ServerState -> CellProp -> ASCell -> IO ()
setPropEndware state (StreamInfo s) c = modifyDaemon state s (c^.cellLocation) evalMsg
  where evalMsg = ServerMessage daemon_message_id $ Evaluate [EvalInstruction (c^.cellExpression) (c^.cellLocation)]
setPropEndware _ _ _ = return ()

removePropEndware :: ServerState -> CellProp -> ASCell -> IO ()
removePropEndware state (StreamInfo s) c = removeDaemon (c^.cellLocation) state
removePropEndware _ _ _ = return ()

-- Given a prop transform, create the new cells by mapping over the transform and run a dispatch cycle with those cells.
-- We want to run a dispatch cycle so that the possibly new formats can propagate; if A1 is now a percent and B1 depended on A1, 
-- then B1 may now have a percent format as well. This isn't necessary for many props, however, (bold doesn't propagate)
-- so a future refactor of props should address this. 
-- Also note that we don't want to re-evaluate the cells for which we're just a adding props; this can, for example, cause random numbers
-- to update when you change their precision. 
transformPropsInDatabase :: MessageId -> (ASCell -> ASCellProps) -> ASUserClient -> ServerState -> ASRange -> IO ()
transformPropsInDatabase mid f uc state rng = do
  let locs = rangeToIndices rng
      conn = state^.dbConn
  cells <- getPossiblyBlankCells conn locs
  -- Create new cells by changing props in accordance with cellPropsTransforms 
  let cells' = map (f >>= (set cellProps)) cells
  -- Update the DB with the new cells, these won't be re-evaluated
  setCells conn cells'
  -- Run a dispatch cycle, but only eval proper descendants, and add the new cells to the update
  errOrUpdate <- DP.runDispatchCycle state cells' ProperDescendants (userCommitSource uc) id
  broadcastErrOrUpdate mid state uc (addCellsToUpdate cells' <$> errOrUpdate)

handleSetProp :: MessageId -> ASUserClient -> ServerState -> CellProp -> ASRange -> IO ()
handleSetProp mid uc state prop rng = transformPropsInDatabase mid (setProp prop . view cellProps) uc state rng

-- Change the decimal precision of all the values in a range
handleChangeDecimalPrecision :: MessageId -> ASUserClient -> ServerState -> Int ->  ASRange -> IO ()
handleChangeDecimalPrecision mid uc state i rng = transformPropsInDatabase mid cellPropsUpdate uc state rng
  where
    getUpdatedFormat :: ASCell -> Maybe Format
    getUpdatedFormat = view format . shiftDecPrecision i . getFormattedVal
    cellPropsUpdate :: ASCell -> ASCellProps
    cellPropsUpdate  = (maybe id (setProp . ValueFormat) . getUpdatedFormat) <*> (view cellProps)
