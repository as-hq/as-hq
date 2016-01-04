module AS.Handlers.Props (handleToggleProp, handleSetProp) where

import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User
import AS.Types.Commits

import AS.DB.API

import AS.Daemon as DM
import AS.Reply
import AS.Util

import Data.List
import Control.Concurrent
import Control.Lens

import Database.Redis (Connection)

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

handleSetProp :: Connection -> ASUserClient -> CellProp -> ASRange -> IO ()
handleSetProp conn uc prop rng = do
  let locs = rangeToIndices rng
  cells <- getPossiblyBlankCells conn locs
  let cells' = map (cellProps %~ setProp prop) cells
  setCells conn cells'
  sendSheetUpdate uc $ sheetUpdateFromCells cells'