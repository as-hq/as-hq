module AS.Handlers.Props (handleToggleProp, handleSetProp, handleChangeDecimalPrecision) where

import Data.List
import Control.Concurrent
import Control.Lens
import qualified Data.Map as M
import Database.Redis (Connection)

import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User
import AS.Types.Commits
import AS.Types.Eval

import AS.DB.API
import AS.Dispatch.Core as DP
import AS.Daemon as DM
import AS.Reply
import AS.Util

-- | Used only for flag props. 
handleToggleProp :: ASUserClient -> MVar ServerState -> CellProp -> ASRange -> IO ()
handleToggleProp uc state p rng = do
  let locs = rangeToIndices rng
      pt   =  propType p
  conn <- view dbConn <$> readMVar state
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
      sendSheetUpdate uc $ sheetUpdateFromCells cells'
    else do
      let cells' = map (cellProps %~ setProp p) cellsWithoutProp
      setCells conn cells'
      mapM_ (setPropEndware state p) cells'
      sendSheetUpdate uc $ sheetUpdateFromCells cells'
    -- don't HAVE to send back the entire cells, but that's an optimization for a later time. 
    -- Said toad. (Alex 11/7)

setPropEndware :: MVar ServerState -> CellProp -> ASCell -> IO ()
setPropEndware state (StreamInfo s) c = modifyDaemon state s (c^.cellLocation) evalMsg
  where evalMsg = ServerMessage $ Evaluate [EvalInstruction (c^.cellExpression) (c^.cellLocation)]
setPropEndware _ _ _ = return ()

removePropEndware :: MVar ServerState -> CellProp -> ASCell -> IO ()
removePropEndware state (StreamInfo s) c = removeDaemon (c^.cellLocation) state
removePropEndware _ _ _ = return ()

-- Given a prop transform, create the new cells by mapping over the transform and run a dispatch cycle with those cells.
-- We want to run a dispatch cycle so that the possibly new formats can propagate; if A1 is now a percent and B1 depended on A1, 
-- then B1 may now have a percent format as well. This isn't necessary for many props, however, (bold doesn't propagate)
-- so a future refactor of props should address this. 
handleTransformProp :: (ASCellProps -> ASCellProps) -> ASUserClient -> MVar ServerState -> ASRange -> IO ()
handleTransformProp f uc state rng = do
  let locs = rangeToIndices rng
  conn <- view dbConn <$> readMVar state
  cells <- getPossiblyBlankCells conn locs
  -- Create new cells by changing props in accordance with cellPropsTransforms 
  let cells' = map (cellProps %~ f) cells
  errOrUpdate <- DP.runDispatchCycle state cells' DescendantsWithParent (userCommitSource uc) id
  broadcastErrOrUpdate state uc errOrUpdate

handleSetProp :: CellProp -> ASUserClient -> MVar ServerState -> ASRange -> IO ()
handleSetProp prop = handleTransformProp (setProp prop)

-- Change the decimal precision of all the values in a range
handleChangeDecimalPrecision :: Int -> ASUserClient -> MVar ServerState -> ASRange -> IO ()
handleChangeDecimalPrecision i = handleTransformProp (upsertProp defaultDecProp updateDecPrecision)
  where
    defaultDecProp = ValueFormat (Format NoFormat (Just i))
    updateDecPrecision (ValueFormat (Format fType Nothing)) = ValueFormat $ Format fType $ Just i
    updateDecPrecision (ValueFormat (Format fType (Just dOff))) = ValueFormat $ Format fType $ Just (dOff + i)
    updateDecPrecision x = x