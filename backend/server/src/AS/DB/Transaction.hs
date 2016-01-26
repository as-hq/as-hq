module AS.DB.Transaction where 

import Prelude()
import AS.Prelude

import AS.Types.Cell
import AS.Types.DB
import AS.Types.Eval
import AS.Types.Errors
import AS.Types.Updates
import AS.Types.Network

import qualified AS.DB.Graph as G
import qualified AS.Serialize as S
import AS.DB.API as DB
import AS.DB.Expanding
import AS.DB.Internal as DI
import AS.Dispatch.Expanding as DE (recomposeCompositeValue)
import AS.Logging
import AS.Util

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe 

import Database.Redis hiding (time)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Control.Concurrent
import Control.Applicative
import Control.Lens hiding (set)

data CommitWithDecoupleInfo = CommitWithDecoupleInfo { baseCommit :: ASCommit, didDecouple :: Bool } deriving (Show)

----------------------------------------------------------------------------------------------------------------------
-- top-level functions

updateDBWithContext :: ServerState -> CommitSource -> EvalContext -> EitherTExec ASCommit
updateDBWithContext state src ctx = 
  let conn = state^.dbConn
      graphAddress = state^.appSettings.graphDbAddress
  in do
    commitWithDecoupleInfo <- lift $ evalContextToCommitWithDecoupleInfo conn (srcSheetId src) ctx
    lift $ pushCommitWithDecoupleInfo graphAddress conn src commitWithDecoupleInfo
    if (didDecouple commitWithDecoupleInfo)
      then left DecoupleAttempt
      else return $ baseCommit commitWithDecoupleInfo

----------------------------------------------------------------------------------------------------------------------
-- conversions

-- The logic for getting the decouple info should ideally be separate from 
-- the one for generating the commit, but by batching them together we get to make one fewer call to 
-- the DB. (Alex 12/24)
evalContextToCommitWithDecoupleInfo :: Connection -> ASSheetId -> EvalContext -> IO CommitWithDecoupleInfo
evalContextToCommitWithDecoupleInfo conn sid (EvalContext mp _ (SheetUpdate cu bu du cfru)) = do
  bdiff  <- updateToDiff bu $ fmap catMaybes . mapM (DB.getBar conn)
  ddiff  <- updateToDiff du $ fmap catMaybes . mapM (DB.getRangeDescriptor conn)
  cfdiff <- updateToDiff cfru $ DB.getCondFormattingRules conn sid
  time   <- getASTime
  -- Doing this manually here instead of using updateToDiff, because we need mOldCells for didDecouple
  let deletedLocs = refsToIndices (oldKeys cu)
      newCells    = L.union (newVals cu) (blankCellsAt deletedLocs)
  mOldCells <- DB.getCells conn $ mapCellLocation newCells
  let cdiff = Diff { beforeVals = catMaybes mOldCells, afterVals = newCells }
      commit = Commit cdiff bdiff ddiff cfdiff time
      didDecouple = any isDecouplePair $ zip mOldCells newCells
      -- the locations in each pair in (zip mOldCells newCells) are the same
      -- determines whether to send a decouple message.
      -- we send a decouple message if there are any decoupled, *visible* cells remaining on the spreadsheet.
      -- i.e. deleting an entire range will not cause a decouple message, but deleting part of it will because 
      -- decoupled cells remain visible on the sheet. 
      -- 
      -- NOTE: if (not $ isBlank acell) for some acell, then: 
      --   * if the entire list was not deleted, then there's going to be some nonblank, noncoupled acell paired with 
      --     a coupled bcell. 
      --   * if the entire list was deleted, then not $ isBlank acell will be false for the entire list, so no decouple
      --     message gets sent. 
      isDecouplePair (mbcell, acell) = case mbcell of 
        Nothing -> False
        Just bcell -> (isCoupled bcell) && (not $ isCoupled acell) && (not $ isBlank acell)
  return $ CommitWithDecoupleInfo commit didDecouple

----------------------------------------------------------------------------------------------------------------------
-- low-level API

-- | Makes sure everything is synced; the listKeys and ancestors in graph db should reflect 
-- the cell changes that happen as a result of setting the cells. 
setCellsPropagated :: GraphAddress -> Connection -> [ASCell] -> IO ()
setCellsPropagated addr conn cells = 
  let roots = filter isEvaluable cells
  in do
    setCells conn cells
    G.setCellsAncestorsForce addr roots

-- | Makes sure everything is synced -- the listKeys and ancestors in graph db should reflect 
-- the cell changes that happen as a result of deleting the cells. 
deleteLocsPropagated :: GraphAddress -> Connection -> [ASIndex] -> IO ()
deleteLocsPropagated addr conn locs = do
  deleteLocs conn locs
  G.removeAncestorsAtForced addr locs

----------------------------------------------------------------------------------------------------------------------
-- Commits

-- #incomplete need to deal with large commit sizes and max number of commits
-- | Return a commit if possible (not possible if you undo past the beginning of time, etc)
-- Update the DB so that there's always a source of truth (ie we will initEval undo to all relevant users)
undo :: GraphAddress -> Connection -> CommitSource -> IO (Maybe ASCommit)
undo addr conn src = do
  let pushKey = toRedisFormat . PushCommitKey $ src
      popKey = toRedisFormat . PopCommitKey $ src
      sid = srcSheetId src
  mCommit <- runRedis conn $ do
    Right commit <- rpoplpush pushKey popKey
    return $ S.maybeDecode =<< commit
  maybe (return Nothing) (liftA2 (>>) (applyUpdateToDBPropagated addr conn sid . sheetUpdateFromCommit . flipCommit) (return . Just)) mCommit

redo :: GraphAddress -> Connection -> CommitSource -> IO (Maybe ASCommit)
redo addr conn src = do
  let pushKey = toRedisFormat . PushCommitKey $ src
      popKey = toRedisFormat . PopCommitKey $ src
      sid = srcSheetId src
  mCommit <- runRedis conn $ do
    Right result <- lpop popKey
    case result of
      Just commit -> do
        rpush pushKey [commit]
        return $ S.maybeDecode commit
      _ -> return Nothing
  maybe (return Nothing) (liftA2 (>>) (applyUpdateToDBPropagated addr conn sid . sheetUpdateFromCommit) (return . Just)) mCommit

applyUpdateToDB :: GraphAddress -> Connection -> ASSheetId -> SheetUpdate -> IO ()
applyUpdateToDB = applyUpdateToDBMaybePropagated False 

applyUpdateToDBPropagated :: GraphAddress -> Connection -> ASSheetId -> SheetUpdate -> IO ()
applyUpdateToDBPropagated = applyUpdateToDBMaybePropagated True

-- internal function 
-- #needsrefactor sheet id is only used for conditional formatting rules, and should be a part of conditional formatting rules. 
applyUpdateToDBMaybePropagated :: Bool -> GraphAddress -> Connection -> ASSheetId -> SheetUpdate -> IO ()
applyUpdateToDBMaybePropagated shouldPropagate addr conn sid u@(SheetUpdate cu bu du cfru) = do 
  let (setCells', deleteLocs') = if shouldPropagate then (setCellsPropagated addr, deleteLocsPropagated addr) else (setCells, deleteLocs)
      allUpdatedCells = L.unionBy isColocated (newVals cu) (blankCellsAt . refsToIndices $ oldKeys cu)
      (emptyCells, nonEmptyCells) = L.partition isEmptyCell allUpdatedCells
  -- don't save blank cells in the database; in fact, we should delete any that are there. 
  deleteLocs' conn $ (mapCellLocation emptyCells)
  setCells' conn nonEmptyCells
  mapM_ (deleteBarAt conn)      (oldKeys bu)
  mapM_ (setBar conn)           (newVals bu)
  mapM_ (deleteDescriptor conn) (oldKeys du)
  mapM_ (setDescriptor conn)    (newVals du)
  deleteCondFormattingRules conn sid $ oldKeys cfru
  setCondFormattingRules conn sid $ newVals cfru
  printObj "applied update to database" u

pushCommit :: Connection -> CommitSource -> ASCommit -> IO ()
pushCommit conn src c = runRedis conn $ do
  let pushKey = toRedisFormat . PushCommitKey $ src
      popKey = toRedisFormat . PopCommitKey $ src
  TxSuccess _ <- multiExec $ do
    rpush pushKey [S.encode c]
    del [popKey]
  return ()

pushCommitWithDecoupleInfo :: GraphAddress -> Connection -> CommitSource -> CommitWithDecoupleInfo -> IO ()
pushCommitWithDecoupleInfo addr conn src commitWithInfo =
  if didDecouple commitWithInfo
    then setTempCommit conn src (baseCommit commitWithInfo)
    else updateDBWithCommit addr conn src (baseCommit commitWithInfo)

-- Do the writes to the DB
updateDBWithCommit :: GraphAddress -> Connection -> CommitSource -> ASCommit -> IO ()
updateDBWithCommit addr conn src c = do 
  applyUpdateToDB addr conn (srcSheetId src) (sheetUpdateFromCommit c)
  pushCommit conn src c

-- Each commit source has a temp commit, used for decouple warnings
-- Key: commitSource + "tempcommit", value: ASCommit bytestring
getTempCommit :: Connection -> CommitSource -> IO (Maybe ASCommit)
getTempCommit conn src = do 
  maybeBStr <- runRedis conn $ $fromRight <$> get (toRedisFormat . TempCommitKey $ src)
  return $ S.maybeDecode =<< maybeBStr
  
setTempCommit :: Connection  -> CommitSource -> ASCommit -> IO ()
setTempCommit conn src c = do 
  runRedis conn $ set (toRedisFormat . TempCommitKey $ src) (S.encode c) 
  return ()
