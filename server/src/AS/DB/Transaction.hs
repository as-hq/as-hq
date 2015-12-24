module AS.DB.Transaction where 

import Prelude

import AS.Types.Cell
import AS.Types.DB
import AS.Types.Eval
import AS.Types.Errors
import AS.Types.Updates

import AS.DB.API as DB
import qualified AS.DB.Graph as G
import AS.DB.Expanding
import AS.DB.Internal as DI
import AS.Dispatch.Expanding as DE (recomposeCompositeValue)
import AS.Logging
import AS.Util

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Serialize as S
import Data.Maybe 

import Database.Redis hiding (time)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Control.Concurrent
import Control.Applicative

data CommitWithInfo = CommitWithInfo { baseCommit :: ASCommit, didDecouple :: Bool } deriving (Show)

----------------------------------------------------------------------------------------------------------------------
-- top-level functions

-- commiTransform is a function to be applied to the commit produced by evalContextToCommit
updateDBWithContext :: Connection -> CommitSource -> EvalContext -> CommitTransform -> EitherTExec ASCommit
updateDBWithContext conn src ctx ctf = do
  commitWithInfo <- lift $ evalContextToCommit conn ctx
  let finalCommit = ctf . baseCommit $ commitWithInfo
  lift $ pushCommitWithInfo conn src (commitWithInfo { baseCommit = finalCommit })
  if (didDecouple commitWithInfo)
    then left DecoupleAttempt
    else return finalCommit

----------------------------------------------------------------------------------------------------------------------
-- conversions

-- || evalContextToCommit gives empty rowcols.
-- | Commit uses bars in sheet as both before and after rowcols.
-- TODO: timchu, 12/14/15. This could be refactored to split off didDecouple
-- and Commit. But we're not because of DB latency.
evalContextToCommit :: Connection -> EvalContext -> IO CommitWithInfo
evalContextToCommit conn (EvalContext mp cells ddiff) = do
  mbcells <- DB.getCells conn (map cellLocation cells)
  time <- getASTime
  let cdiff   = Diff { beforeVals = (catMaybes mbcells), afterVals = cells }
      commit  = (emptyCommitWithTime time) { cellDiff = cdiff, rangeDescriptorDiff = ddiff }
      didDecouple = any isDecouplePair $ zip mbcells cells
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
  printObj "DEALING WITH DECOUPLING OF DESCRIPTORS " $ beforeVals ddiff
  return $ CommitWithInfo commit didDecouple

----------------------------------------------------------------------------------------------------------------------
-- low-level API

  -- | Makes sure everything is synced -- the listKeys and ancestors in graph db should reflect 
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

-- Commits

-- TODO: need to deal with large commit sizes and max number of commits
-- TODO: need to delete blank cells from the DB. (Otherwise e.g. if you delete a
-- a huge range, you're going to have all those cells in the DB doing nothing.)

-- | Return a commit if possible (not possible if you undo past the beginning of time, etc)
-- | Update the DB so that there's always a source of truth (ie we will initEval undo to all relevant users)
undo :: Connection -> CommitSource -> IO (Maybe ASCommit)
undo conn src = do
  let pushKey = toRedisFormat . PushCommitKey $ src
      popKey = toRedisFormat . PopCommitKey $ src
      sid = srcSheetId src
  mCommit <- runRedis conn $ do
    Right commit <- rpoplpush pushKey popKey
    return $ maybeDecode =<< commit
  maybe (return Nothing) (liftA2 (>>) (applyUpdateToDBPropagated conn sid . sheetUpdateFromCommit . flipCommit) (return . Just)) mCommit

redo :: Connection -> CommitSource -> IO (Maybe ASCommit)
redo conn src = do
  let pushKey = toRedisFormat . PushCommitKey $ src
      popKey = toRedisFormat . PopCommitKey $ src
      sid = srcSheetId src
  mCommit <- runRedis conn $ do
    Right result <- lpop popKey
    case result of
      Just commit -> do
        rpush pushKey [commit]
        return $ maybeDecode commit
      _ -> return Nothing
  maybe (return Nothing) (liftA2 (>>) (applyUpdateToDBPropagated conn sid . sheetUpdateFromCommit) (return . Just)) mCommit

applyUpdateToDB :: Connection -> ASSheetId -> SheetUpdate -> IO ()
applyUpdateToDB = applyUpdateToDBMaybePropagated False 

applyUpdateToDBPropagated :: Connection -> ASSheetId -> SheetUpdate -> IO ()
applyUpdateToDBPropagated = applyUpdateToDBMaybePropagated True

-- internal function 
-- #needsrefactor sheet id is only used for conditional formatting rules, and should be a part of 
-- conditional formatting rules. 
applyUpdateToDBMaybePropagated :: Bool -> Connection -> ASSheetId -> SheetUpdate -> IO ()
applyUpdateToDBMaybePropagated shouldPropagate conn sid (SheetUpdate cu bu du cfru) = do 
  let fromIndexRef (IndexRef i) = i
      (setCells', deleteLocs') = if shouldPropagate then (setCellsPropagated, deleteLocsPropagated) else (setCells, deleteLocs)
      (emptyCells, nonEmptyCells) = L.partition isEmptyCell $ newVals cu 
  setCells' conn nonEmptyCells
  -- don't save blank cells in the database; in fact, we should delete any that are there. 
  deleteLocs' conn $ (map cellLocation emptyCells) ++ (map fromIndexRef $ oldKeys cu) -- ::ALEX:: should change the type of ASCell's key from ASReference to a new type
  mapM_ (deleteBarAt conn)      (oldKeys bu)
  mapM_ (setBar conn)           (newVals bu)
  mapM_ (deleteDescriptor conn) (oldKeys du)
  mapM_ (setDescriptor conn)    (newVals du)
  deleteCondFormattingRules conn sid $ oldKeys cfru
  setCondFormattingRules conn sid $ newVals cfru

pushCommit :: Connection -> CommitSource -> ASCommit -> IO ()
pushCommit conn src c = runRedis conn $ do
  let pushKey = toRedisFormat . PushCommitKey $ src
      popKey = toRedisFormat . PopCommitKey $ src
  TxSuccess _ <- multiExec $ do
    rpush pushKey [S.encode c]
    del [popKey]
  return ()

pushCommitWithInfo :: Connection -> CommitSource -> CommitWithInfo -> IO ()
pushCommitWithInfo conn src commit =
  if didDecouple commit
    then setTempCommit conn src (baseCommit commit)
    else updateDBWithCommit conn src (baseCommit commit)

-- Do the writes to the DB
updateDBWithCommit :: Connection -> CommitSource -> ASCommit -> IO ()
updateDBWithCommit conn src c = do 
  applyUpdateToDB conn (srcSheetId src) (sheetUpdateFromCommit c)
  pushCommit conn src c

-- Each commit source has a temp commit, used for decouple warnings
-- Key: commitSource + "tempcommit", value: ASCommit bytestring
getTempCommit :: Connection -> CommitSource -> IO (Maybe ASCommit)
getTempCommit conn src = do 
  maybeBStr <- runRedis conn $ fromRight <$> get (toRedisFormat . TempCommitKey $ src)
  return $ maybeDecode =<< maybeBStr
  
setTempCommit :: Connection  -> CommitSource -> ASCommit -> IO ()
setTempCommit conn src c = do 
  runRedis conn $ set (toRedisFormat . TempCommitKey $ src) (S.encode c) 
  return ()