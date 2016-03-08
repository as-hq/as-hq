module AS.DB.Transaction where 

import Prelude()
import AS.Prelude

import AS.Types.Cell
import AS.Types.DB
import AS.Types.Eval
import AS.Types.Errors
import AS.Types.Updates
import AS.Types.Network
import AS.Types.Commits

import qualified AS.DB.Graph as G
import qualified AS.Serialize as S
import AS.DB.API as DB
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

updateDBWithContext :: Connection -> CommitSource -> EvalContext -> EitherTExec ASCommit
updateDBWithContext conn src ctx = do
  commitWithDecoupleInfo <- lift $ evalContextToCommitWithDecoupleInfo conn (srcSheetId src) ctx
  lift $ pushCommitWithDecoupleInfo conn src commitWithDecoupleInfo
  if didDecouple commitWithDecoupleInfo
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
  let deletedLocs = refsToIndices (cu^.oldKeys)
      newCells    = L.union (cu^.newVals) (blankCellsAt deletedLocs)
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
setCellsPropagated :: Connection -> [ASCell] -> IO ()
setCellsPropagated conn cells = 
  let roots = filter isEvaluable cells
  in do
    setCells conn cells
    G.setCellsAncestorsForce roots

-- | Makes sure everything is synced -- the listKeys and ancestors in graph db should reflect 
-- the cell changes that happen as a result of deleting the cells. 
deleteLocsPropagated :: Connection -> [ASIndex] -> IO ()
deleteLocsPropagated conn locs = do
  deleteLocs conn locs
  G.removeAncestorsAtForced locs

----------------------------------------------------------------------------------------------------------------------
-- Commits

-- #incomplete need to deal with large commit sizes and max number of commits
-- | Return a commit if possible (not possible if you undo past the beginning of time, etc)
-- Update the DB so that there's always a source of truth (ie we will initEval undo to all relevant users)
undo :: Connection -> CommitSource -> IO (Maybe ASCommit)
undo conn src = do
  let pushKey = S.encode $ PushCommitKey $ src
      popKey = S.encode $ PopCommitKey $ src
      sid = srcSheetId src
  mCommit <- runRedis conn $ do
    Right commit <- rpoplpush pushKey popKey
    return $ commit >>= S.maybeDecode >>= dbValToCommit
  maybe (return Nothing) (liftA2 (>>) (applyUpdateToDBPropagated conn sid . sheetUpdateFromCommit . flipCommit) (return . Just)) mCommit

redo :: Connection -> CommitSource -> IO (Maybe ASCommit)
redo conn src = do
  let pushKey = S.encode $ PushCommitKey $ src
      popKey = S.encode $ PopCommitKey $ src
      sid = srcSheetId $ src
  mCommit <- runRedis conn $ do
    Right result <- lpop popKey
    case result of
      Just commit -> do
        rpush pushKey [commit]
        return $ S.maybeDecode commit >>= dbValToCommit
      Nothing -> return Nothing
  maybe (return Nothing) (liftA2 (>>) (applyUpdateToDBPropagated conn sid . sheetUpdateFromCommit) (return . Just)) mCommit

applyUpdateToDB :: Connection -> ASSheetId -> SheetUpdate -> IO ()
applyUpdateToDB = applyUpdateToDBMaybePropagated False 

applyUpdateToDBPropagated :: Connection -> ASSheetId -> SheetUpdate -> IO ()
applyUpdateToDBPropagated = applyUpdateToDBMaybePropagated True

-- internal function 
-- #needsrefactor sheet id is only used for conditional formatting rules, and should be a part of conditional formatting rules. 
applyUpdateToDBMaybePropagated :: Bool -> Connection -> ASSheetId -> SheetUpdate -> IO ()
applyUpdateToDBMaybePropagated shouldPropagate conn sid u@(SheetUpdate cu bu du cfru) = do 
  let (setCells', deleteLocs') = if shouldPropagate then (setCellsPropagated, deleteLocsPropagated) else (setCells, deleteLocs)
      allUpdatedCells = L.unionBy isColocated (cu^.newVals) (blankCellsAt . refsToIndices $ cu^.oldKeys)
      (emptyCells, nonEmptyCells) = L.partition isEmptyCell allUpdatedCells
  -- don't save blank cells in the database; in fact, we should delete any that are there. 
  deleteLocs' conn $ (mapCellLocation emptyCells)
  setCells' conn nonEmptyCells
  mapM_ (deleteBarAt conn)      (bu^.oldKeys)
  mapM_ (setBar conn)           (bu^.newVals)
  mapM_ (deleteDescriptor conn) (du^.oldKeys)
  mapM_ (setDescriptor conn)    (du^.newVals)
  deleteCondFormattingRules conn sid $ cfru^.oldKeys
  setCondFormattingRules conn sid $ cfru^.newVals
  printObj "applied update to database" u

pushCommit :: Connection -> CommitSource -> ASCommit -> IO ()
pushCommit conn src c = runRedis conn $ do
  let pushKey = S.encode $ PushCommitKey $ src
      popKey = S.encode $ PopCommitKey $ src
      sid = srcSheetId src
  TxSuccess _ <- multiExec $ do
    rpush pushKey [S.encode $ CommitValue c]
    del [popKey]
  return ()

pushCommitWithDecoupleInfo :: Connection -> CommitSource -> CommitWithDecoupleInfo -> IO ()
pushCommitWithDecoupleInfo conn src commitWithInfo =
  if didDecouple commitWithInfo
    then setTempCommit conn src (baseCommit commitWithInfo)
    else updateDBWithCommit conn src (baseCommit commitWithInfo)

-- | Both saves an ASCommit in the database for future undo/redo, and applies the diff (i.e. replace
-- old values with new ones, as specified in the commit.)
updateDBWithCommit :: Connection -> CommitSource -> ASCommit -> IO ()
updateDBWithCommit conn src c = do 
  applyUpdateToDB conn (srcSheetId src) (sheetUpdateFromCommit c)
  pushCommit conn src c

