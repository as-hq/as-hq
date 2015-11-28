module AS.Handlers.JumpSelect (handleJumpSelect) where

import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User

import AS.Util
import AS.DB.API as DB
import AS.Config.Settings as CS
import AS.Reply

import Database.Redis
import Data.Bits (xor)
import Control.Concurrent

-- #incomplete the logic here is actually wrong. Also isn't hooked up to anything on frontend yet. 
handleJumpSelect :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleJumpSelect uc state p@(PayloadJump sel origin shifted dir) = 
  let 
    (Range sid (tl, br)) = sel
    (Index _ orig) = origin
    isOrientedH = (col orig) == (col tl)
    isOrientedV = (row orig) == (row tl)
    hExtremum = if isOrientedH then (col br) else (col tl)
    vExtremum = if isOrientedV then (row br) else (row tl)

    jumpSelect :: Connection -> [Int] -> Bool -> IO (Maybe ASReference)
    jumpSelect _ [] _ = return Nothing
    jumpSelect conn (iter:iters) startExists = case dir of
      DRight -> do
        thisExists <- DB.locationExists conn (Index sid (iter,row orig))
        let idx = if thisExists then (iter,row tl) else (iter-1,row tl)
            resultCol = if isOrientedH then (col tl) else (col idx)
            resultCol2 = if isOrientedH then (col idx) else (col br)
            result = if shifted
              then Just . RangeRef . orientRange $ Range sid ((resultCol,row tl), (resultCol2,row br))
              else Just . IndexRef $ Index sid idx
        if (xor startExists thisExists) 
          then return result 
          else jumpSelect conn iters startExists
              
      DDown -> do
        thisExists <- DB.locationExists conn (Index sid (col orig,iter))
        let idx = if thisExists then (col tl,iter) else (col tl,iter-1)
            resultRow = if isOrientedV then (row tl) else (row idx)
            resultRow2 = if isOrientedV then (row idx) else (row br)
            result = if shifted
              then Just . RangeRef . orientRange $ Range sid ((col tl,resultRow), (col br,resultRow2))
              else Just . IndexRef $ Index sid idx
        if (xor startExists thisExists) 
          then return result 
          else jumpSelect conn iters startExists

      DLeft -> do
        thisExists <- DB.locationExists conn (Index sid (iter,row orig))
        let idx = if thisExists then (iter,row tl) else (iter+1,row tl)
            resultCol = if isOrientedH then (col tl) else (col idx)
            resultCol2 = if isOrientedH then (col idx) else (col br)
            result = if shifted
              then Just . RangeRef . orientRange $ Range sid ((resultCol,row tl), (resultCol2,row br))
              else Just . IndexRef $ Index sid idx
        if (xor startExists thisExists) 
          then return result 
          else jumpSelect conn iters startExists

      DUp -> do
        thisExists <- DB.locationExists conn (Index sid (col orig,iter))
        let idx = if thisExists then (col tl,iter) else (col tl,iter+1)
            resultRow = if isOrientedV then (row tl) else (row idx)
            resultRow2 = if isOrientedV then (row idx) else (row br)
            result = if shifted
              then Just . RangeRef . orientRange $ Range sid ((col tl,resultRow), (col br,resultRow2))
              else Just . IndexRef $ Index sid idx
        if (xor startExists thisExists) 
          then return result 
          else jumpSelect conn iters startExists
              
  in do
    conn <- dbConn <$> readMVar state
    newSel <- case dir of 
      DRight -> do
        nextExists <- DB.locationExists conn (Index sid (hExtremum+1,row tl))
        let startCol = if nextExists then hExtremum else (hExtremum + 1)
        startExists <- DB.locationExists conn (Index sid (startCol,row tl))
        let iterCols = [startCol..CS.largeSearchBound]
        maybeNewSel <- jumpSelect conn iterCols startExists
        return $ case maybeNewSel of 
          Nothing -> RangeRef sel
          (Just newSel) -> newSel 
      DDown -> do
        nextExists <- DB.locationExists conn (Index sid (col tl,vExtremum+1))
        let startRow = if nextExists then vExtremum else (vExtremum + 1)
        startExists <- DB.locationExists conn (Index sid (col tl,startRow))
        let iterRows = [startRow..CS.largeSearchBound]
        maybeNewSel <- jumpSelect conn iterRows startExists
        return $ case maybeNewSel of 
          Nothing -> RangeRef sel
          (Just newSel) -> newSel 
      DLeft -> do
        nextExists <- DB.locationExists conn (Index sid (hExtremum-1,row tl))
        let startCol = if nextExists then hExtremum else (hExtremum - 1)
        startExists <- DB.locationExists conn (Index sid (startCol,row tl))
        let iterCols = reverse [1..startCol]
        maybeNewSel <- jumpSelect conn iterCols startExists
        return $ case maybeNewSel of 
          Nothing -> RangeRef $ Range sid ((1,row tl), (col2,row2))
            where
              col2 = if shifted then (if isOrientedH then (col tl) else (col br)) else 1
              row2 = if shifted then (row br) else (row tl)
          (Just newSel) -> newSel 
      DUp -> do
        nextExists <- DB.locationExists conn (Index sid (col tl,vExtremum-1))
        let startRow = if nextExists then vExtremum else (vExtremum - 1)
        startExists <- DB.locationExists conn (Index sid (col tl,startRow))
        let iterRows = reverse [1..startRow]
        maybeNewSel <- jumpSelect conn iterRows startExists
        return $ case maybeNewSel of 
          Nothing -> RangeRef $ Range sid ((col tl,1), (col2,row2))
            where
              col2 = if shifted then (col br) else (col tl)
              row2 = if shifted then (if isOrientedV then (row tl) else (row br)) else 1
          (Just newSel) -> newSel 
    let (newSel', newOrigin) = case newSel of 
                              RangeRef r@(Range _ _) -> (r, origin)
                              IndexRef i@(Index _ ind) -> (Range sid (ind,ind), i)
    sendToOriginal uc $ ServerMessage JumpSelect Success (PayloadSelection newSel' newOrigin)