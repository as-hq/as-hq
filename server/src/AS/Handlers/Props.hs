module AS.Handlers.Props (handleToggleProp, handleSetProp) where

import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User

import AS.DB.API

import AS.Daemon as DM
import AS.Reply
import AS.Util

import Data.List
import Control.Concurrent

-- | Used only for flag props. 
handleToggleProp :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleToggleProp uc state (PayloadProp p rng) = do
  let locs = rangeToIndices rng
      pt   =  propType p
  conn <- dbConn <$> readMVar state
  cells <- getPossiblyBlankCells conn locs
  let (cellsWithProp, cellsWithoutProp) = partition ((hasPropType pt) . cellProps) cells
  -- if there's a single prop present in the range, remove this prop from all the cells; 
  -- otherwise set the prop in all the cells. 
  if (null cellsWithoutProp)
    then do 
      let cells' = map (\(Cell l e v ps) -> Cell l e v (removeProp pt ps)) cellsWithProp
          (emptyCells, nonEmptyCells) = partition isEmptyCell cells'
      conn <- dbConn <$> readMVar state
      setCells conn nonEmptyCells
      deleteLocs conn $ map cellLocation emptyCells
      mapM_ (removePropEndware state p) nonEmptyCells
      broadcastFiltered state uc $ makeReplyMessageFromCells Update cells'
    else do
      let cells' = map (\(Cell l e v ps) -> Cell l e v (setProp p ps)) cellsWithoutProp
      conn <- dbConn <$> readMVar state
      setCells conn cells'
      mapM_ (setPropEndware state p) cells'
      broadcastFiltered state uc $ makeReplyMessageFromCells Update cells'
    -- don't HAVE to send back the entire cells, but that's an optimization for a later time. 
    -- Said toad. (Alex 11/7)

setPropEndware :: MVar ServerState -> CellProp -> ASCell -> IO ()
setPropEndware state (StreamInfo s) c = modifyDaemon state s (cellLocation c) evalMsg
  where evalMsg = ClientMessage Evaluate (PayloadCL [c])
setPropEndware _ _ _ = return ()

removePropEndware :: MVar ServerState -> CellProp -> ASCell -> IO ()
removePropEndware state (StreamInfo s) c = removeDaemon (cellLocation c) state
removePropEndware _ _ _ = return ()

handleSetProp :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleSetProp uc state (PayloadProp prop rng) = do
  curState <- readMVar state
  let locs = rangeToIndices rng
  cells <- getPossiblyBlankCells (dbConn curState) locs
  let cells' = map (\(Cell l e v oldProps) -> Cell l e v (setProp prop oldProps)) cells
  setCells (dbConn curState) cells'
  broadcastFiltered state uc $ makeReplyMessageFromCells Update cells'