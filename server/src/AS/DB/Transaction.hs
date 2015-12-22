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

data CommitWithInfo = CommitWithInfo { baseCommit :: ASCommit, didDecouple :: Bool }

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
      bardiff = emptyDiff
      commit  = Commit bardiff cdiff ddiff time
      rd      = beforeVals ddiff
      didDecouple = any isDecouplePair $ zip mbcells cells
      -- determines whether to send a decouple message.
      -- we send a decouple message if there are any decoupled, *visible* cells remaining on the spreadsheet.
      -- i.e. deleting an entire range will not cause a decouple message, but deleting part of it will because 
      -- decoupled cells remain visible on the sheet.
      isDecouplePair (mbcell, acell) = case mbcell of 
        Nothing -> False
        Just bcell -> (isCoupled bcell) && (not $ isCoupled acell) && (not $ isBlank acell)
  printWithTime $ "DEALING WITH DECOUPLING OF DESCRIPTORS " ++ (show rd)
  return $ CommitWithInfo commit didDecouple

----------------------------------------------------------------------------------------------------------------------
-- low-level API

  -- | Makes sure everything is synced -- the listKeys and ancestors in graph db should reflect 
-- the cell changes that happen as a result of setting the cells. 
setCellsPropagated :: Connection -> [ASCell] -> [RangeDescriptor] -> IO ()
setCellsPropagated conn cells descs = 
  let roots = filter isEvaluable cells
  in do
    setCells conn cells
    G.setCellsAncestorsForce roots
    mapM_ (setDescriptor conn) descs

-- | Makes sure everything is synced -- the listKeys and ancestors in graph db should reflect 
-- the cell changes that happen as a result of deleting the cells. 
deleteLocsPropagated :: Connection -> [ASIndex] -> [RangeDescriptor] -> IO ()
deleteLocsPropagated conn locs descs = do
  deleteLocs conn locs
  G.removeAncestorsAtForced locs
  mapM_ (deleteDescriptor conn) descs

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
  commit <- runRedis conn $ do
    Right commit <- rpoplpush pushKey popKey
    return $ decodeMaybe =<< commit
  case commit of
    Nothing -> return Nothing
    Just c@(Commit bardiff cdiff ddiff t) -> do
      deleteLocsPropagated conn (map cellLocation $ afterVals cdiff) (afterVals ddiff)
      setCellsPropagated conn (beforeVals cdiff) (beforeVals ddiff)
      DB.replaceBars conn (afterVals bardiff) (beforeVals bardiff)
      return $ Just c

redo :: Connection -> CommitSource -> IO (Maybe ASCommit)
redo conn src = do
  let pushKey = toRedisFormat . PushCommitKey $ src
      popKey = toRedisFormat . PopCommitKey $ src
  commit <- runRedis conn $ do
    Right result <- lpop popKey
    case result of
      Just commit -> do
        rpush pushKey [commit]
        return $ decodeMaybe commit
      _ -> return Nothing
  case commit of
    Nothing -> return Nothing
    Just c@(Commit bardiff cdiff ddiff t) -> do
      deleteLocsPropagated conn (map cellLocation $ beforeVals cdiff) (beforeVals ddiff)
      setCellsPropagated conn (afterVals cdiff) (afterVals ddiff)
      DB.replaceBars conn (beforeVals bardiff) (afterVals bardiff)
      return $ Just c

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
updateDBWithCommit conn src c@(Commit bardiff cdiff ddiff time) = do 
  -- update cells
  let arc = afterVals bardiff
      af = afterVals cdiff
  DB.setCells conn af
  deleteLocs conn $ map cellLocation $ filter isEmptyCell af
  mapM_ (setDescriptor conn) (afterVals ddiff)
  mapM_ (deleteDescriptor conn) (beforeVals ddiff)
  -- update Rows and Columns in sheet
  DB.replaceBars conn (beforeVals bardiff) (afterVals bardiff)
  pushCommit conn src c

-- Each commit source has a temp commit, used for decouple warnings
-- Key: commitSource + "tempcommit", value: ASCommit bytestring
getTempCommit :: Connection -> CommitSource -> IO (Maybe ASCommit)
getTempCommit conn src = do 
  maybeBStr <- runRedis conn $ fromRight <$> get (toRedisFormat . TempCommitKey $ src)
  return $ decodeMaybe =<< maybeBStr
  
setTempCommit :: Connection  -> CommitSource -> ASCommit -> IO ()
setTempCommit conn src c = do 
  runRedis conn $ set (toRedisFormat . TempCommitKey $ src) (S.encode c) 
  return ()