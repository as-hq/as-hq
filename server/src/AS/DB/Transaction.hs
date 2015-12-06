module AS.DB.Transaction where

import Prelude

import AS.Types.Cell
import AS.Types.DB
import AS.Types.Eval
import AS.Types.Errors

import AS.DB.API as DB
import qualified AS.DB.Graph as G
import AS.DB.Expanding
import AS.DB.Internal as DI
import AS.Dispatch.Expanding as DE (recomposeCompositeValue)
import AS.Logging

import Database.Redis hiding (time)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Data.List (nub)
import Data.Maybe 
import qualified Data.Map as M
import Data.List

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Control.Concurrent

-- used by lookUpRef
referenceToCompositeValue :: Connection -> EvalContext -> ASReference -> IO CompositeValue
referenceToCompositeValue _ (EvalContext mp _ _) (IndexRef i) = return $ CellValue . cellValue $ mp M.! i 
referenceToCompositeValue conn ctx (PointerRef p) = do 
  let idx = pointerToIndex p
  let mp = contextMap ctx
  let cell = mp M.! idx
  case (cellExpression cell) of
    Expression _ _ -> error "Pointer to normal expression, flipping a shit" 
    Coupled _ _ expType rKey -> do 
      mDescriptor <- getRangeDescriptorUsingContext conn ctx rKey
      case mDescriptor of
        Nothing -> error "Couldn't find range descriptor of coupled expression, flipping a shit"
        Just descriptor -> return $ DE.recomposeCompositeValue fatCell
          where
            indices = rangeKeyToIndices rKey
            cells   = map ((contextMap ctx) M.!) indices
            fatCell = FatCell cells descriptor
referenceToCompositeValue conn ctx (RangeRef r) = return . Expanding . VList . M $ vals
  where
    indices = rangeToIndicesRowMajor2D r
    indToVal ind = cellValue $ (contextMap ctx) M.! ind
    vals    = map (map indToVal) indices


----------------------------------------------------------------------------------------------------------------------
-- top-level functions

---- takes in [new cells], [new fat cells] as a result of evalChain, and returns the list 
---- of locations decoupled as a result.
--getDecouplingEffects :: Connection -> ASSheetId -> [ASCell] -> [FatCell] -> IO ([ASIndex])
--getDecouplingEffects conn sid cells fcells = 
--  let locs = map cellLocation cells
--      keys = map (descriptorKey . descriptor) fcells
--  in do
--    rangeKeysChangedByCells    <- liftIO $ DI.getFatCellIntersections conn sid (Left locs)
--    rangeKeysChangedByFatCells <- liftIO $ DI.getFatCellIntersections conn sid (Right keys)
--    let rangeKeysChanged = rangeKeysChangedByCells ++ rangeKeysChangedByFatCells
--    let decoupledLocs    = concat $ map DI.rangeKeyToIndices rangeKeysChanged
--    return decoupledLocs

-- After getting the final context, update the DB and return the cells changed by the entire eval
updateDBFromEvalContext :: Connection -> CommitSource -> EvalContext -> EitherTExec [ASCell]
updateDBFromEvalContext conn src (EvalContext mp cells ddiff) = do
  bcells <- lift $ catMaybes <$> DB.getCells (map cellLocation cells)
  time <- lift $ getASTime
  let cdiff   = CellDiff { beforeCells = bcells, afterCells = cells}
      commit  = Commit cdiff ddiff time
      rd      = removedDescriptors ddiff
  if (length rd == 0) -- there were no decoupled cells
    then do 
      lift $ updateDBAfterEval conn src commit
      return cells
    else do
      printWithTimeT $ "DEALING WITH DECOUPLING OF DESCRIPTORS " ++ (show rd)
      let rangeKeysRemoved = map descriptorKey rd
      coupledCells <- lift $ concat <$> (mapM (getCellsBeforeDecoupling conn) rangeKeysRemoved)
      printWithTimeT $ "COUPLED CELLS " ++ (show coupledCells)
      liftIO $ setTempCommit conn commit src
      liftIO $ setRangeKeysChanged conn rangeKeysRemoved src
      left $ DecoupleAttempt
      
-- Do the writes to the DB
updateDBAfterEval :: Connection -> CommitSource -> ASCommit -> IO ()
updateDBAfterEval conn src c@(Commit cdiff ddiff time) = do 
  let af = afterCells cdiff
  DB.setCells af
  deleteCells conn $ filter isEmptyCell af
  mapM_ (couple conn) (addedDescriptors ddiff)
  pushCommit conn c src

----------------------------------------------------------------------------------------------------------------------
-- helpers

  -- | Makes sure everything is synced -- the listKeys and ancestors in graph db should reflect 
-- the cell changes that happen as a result of setting the cells. 
setCellsPropagated :: Connection -> [ASCell] -> [RangeDescriptor] -> IO ()
setCellsPropagated conn cells descs = 
  let roots = filter (\c -> (not $ isFatCellMember c) || isFatCellHead c) cells
  in do
    setCells cells
    G.setCellsAncestorsForce roots
    mapM_ (couple conn) descs

-- | Makes sure everything is synced -- the listKeys and ancestors in graph db should reflect 
-- the cell changes that happen as a result of deleting the cells. 
deleteCellsPropagated :: Connection -> [ASCell] -> [RangeDescriptor] -> IO ()
deleteCellsPropagated conn cells descs = do
  deleteCells conn cells
  G.removeAncestorsAtForced $ map cellLocation cells
  mapM_ (decouple conn) $ map descriptorKey descs

----------------------------------------------------------------------------------------------------------------------
-- Commits

-- TODO: need to deal with large commit sizes and max number of commits
-- TODO: need to delete blank cells from the DB. (Otherwise e.g. if you delete a
-- a huge range, you're going to have all those cells in the DB doing nothing.)

pushKey :: CommitSource -> B.ByteString
pushKey (sid, uid) = B.pack $ (T.unpack sid) ++ '|':(T.unpack uid) ++ "pushed"

popKey :: CommitSource -> B.ByteString
popKey (sid, uid)  = B.pack $ (T.unpack sid) ++ '|':(T.unpack uid) ++ "popped"

-- | Return a commit if possible (not possible if you undo past the beginning of time, etc)
-- | Update the DB so that there's always a source of truth (ie we will initEval undo to all relevant users)
undo :: Connection -> CommitSource -> IO (Maybe ASCommit)
undo conn src = do
  commit <- runRedis conn $ do
    (Right commit) <- rpoplpush (pushKey src) (popKey src)
    return $ DI.bStrToASCommit commit
  case commit of
    Nothing -> return Nothing
    Just c@(Commit cdiff ddiff t) -> do
      deleteCellsPropagated conn (afterCells cdiff) (addedDescriptors ddiff)
      setCellsPropagated conn (beforeCells cdiff) (removedDescriptors ddiff)
      return $ Just c

redo :: Connection -> CommitSource -> IO (Maybe ASCommit)
redo conn src = do
  commit <- runRedis conn $ do
    Right result <- lpop (popKey src)
    case result of
      Just commit -> do
        rpush (pushKey src) [commit]
        return $ DI.bStrToASCommit (Just commit)
      _ -> return Nothing
  case commit of
    Nothing -> return Nothing
    Just c@(Commit cdiff ddiff t) -> do
      deleteCellsPropagated conn (beforeCells cdiff) (removedDescriptors ddiff)
      setCellsPropagated conn (afterCells cdiff) (addedDescriptors ddiff)
      return $ Just c

pushCommit :: Connection -> ASCommit -> CommitSource -> IO ()
pushCommit conn c src = do
  let commit = (B.pack . show) c
  runRedis conn $ do
    TxSuccess _ <- multiExec $ do
      rpush (pushKey src) [commit]
      incrbyfloat "numCommits" 1
      del [popKey src]
    return ()

-- Each commit source has a temp commit, used for decouple warnings
-- Key: commitSource + "tempcommit", value: ASCommit bytestring
getTempCommit :: Connection -> CommitSource -> IO (Maybe ASCommit)
getTempCommit conn src = do 
  let commitSource = B.pack $ (show src) ++ "tempcommit"
  maybeBStr <- runRedis conn $ do
    TxSuccess c <- multiExec $ do
      get commitSource
    return c
  return $ bStrToASCommit maybeBStr
  

setTempCommit :: Connection  -> ASCommit -> CommitSource -> IO ()
setTempCommit conn c src = do 
  let commit = (B.pack . show) c 
  let commitSource = B.pack $ (show src) ++ "tempcommit"
  runRedis conn $ do
    TxSuccess _ <- multiExec $ do
      set commitSource commit
    return ()
