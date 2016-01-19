module AS.Handlers.Props (handleToggleProp, handleSetProp) where

import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User
import AS.Types.Commits
import AS.Types.BarProps (BarProp(FromCell))
import AS.Types.Bar

import AS.DB.API

import AS.Daemon as DM
import AS.Reply
import AS.Util
import AS.Handlers.Misc

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

-- TODO: This will be implemented once extended ranges have been implemented.
isColRange :: ASRange -> Bool
isColRange _ = False

-- TODO: Timchu. This is the partial logic for creating a column range and updating
-- the bar props accordingly.

--TODO: Timchu, update handle set bar prop.
handleSetProp :: ASUserClient -> ServerState -> CellProp -> ASRange -> IO ()
handleSetProp uc state prop rng = do
  let conn = state^.dbConn
      graphAddress = state^.appSettings.graphDbAddress
      locs = rangeToIndices rng
  if isColRange rng
     then do
       let sid = rangeSheetId rng
           bInt1 = (fst $ range rng)^.col -- TODO: rng^.rect^.tl^.col^.val
           bInt2 = (snd $ range rng)^.row -- TODO: toFinite $ rng^.rect^.br^.extendedCol^.val
           bProp = FromCell prop
           bars = map (BarIndex sid ColumnType) [bInt1..bInt2]
       updateAndCommitBars uc graphAddress conn bProp $ bars
       sendToOriginal uc $ ClientMessage NoAction
     else do
       cells <- getPossiblyBlankCells conn locs
       let cells' = map (cellProps %~ setProp prop) cells
       setCells conn cells'
     -- Do I want to be sending a sheet update?
       sendSheetUpdate uc $ sheetUpdateFromCells cells'
