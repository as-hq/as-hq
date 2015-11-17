module AS.DB.Transaction where

import Prelude

import AS.Types.Core
import AS.Types.DB
import AS.DB.API as DB
import AS.DB.Util as DU
import AS.Util as U

import Database.Redis
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Data.List (nub)
import Data.Maybe (catMaybes)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class

----------------------------------------------------------------------------------------------------------------------
-- top-level functions

-- | Deal with updating all DB-related things after an eval. 
writeTransaction :: Connection -> ASTransaction -> EitherTExec [ASCell]
writeTransaction conn (Transaction src@(sid, _) roots afterCells fatCells) = 
  let extraCells      = concat $ map expandedCells fatCells
      locs            = map cellLocation afterCells
      afterCells'     = afterCells ++ extraCells
      locs'           = map cellLocation afterCells'
      rangeKeys       = map (rangeDescriptorToKey . descriptor) fatCells
  in do
    beforeCells <- lift $ catMaybes <$> DB.getCells locs'
    -- determine all fatcell intersections produced by eval
    rangeKeysChangedByCells <- liftIO $ DU.getFatCellIntersections conn sid (Left locs)
    rangeKeysChangedByFatCells <- liftIO $ DU.getFatCellIntersections conn sid (Right rangeKeys)
    let rangeKeysChanged = rangeKeysChangedByCells ++ rangeKeysChangedByFatCells
    -- decouple the intersected fat cells
    coupledCells <- lift $ concat <$> (mapM (decouple conn) rangeKeysChanged)
    let decoupledCells  = map DU.decoupleCell coupledCells
        afterCells''    = U.mergeCells afterCells' decoupledCells
        beforeCells'    = U.mergeCells beforeCells coupledCells
    -- set the database
    liftIO $ DB.setCells afterCells''
    -- delete empty cells after setting them
    liftIO $ deleteCells conn $ filter isEmptyCell afterCells''
    liftIO $ mapM_ (couple conn . descriptor) fatCells
    liftIO $ addCommit conn (beforeCells', afterCells'') src
    liftIO $ printWithTime "added commits"
    right afterCells'

----------------------------------------------------------------------------------------------------------------------
-- helpers

couple :: Connection -> RangeDescriptor -> IO ()
couple conn desc = 
  let rangeKey       = DU.rangeDescriptorToKey desc 
      rangeKey'      = id $! B.pack rangeKey 
      sheetRangesKey = DU.getSheetRangesKey $ DU.rangeKeyToSheetId rangeKey
      rangeValue     = B.pack $ show desc
  in runRedis conn $ do
      liftIO $ printWithTime $ "setting list locations for key: " ++ rangeKey
      set rangeKey' rangeValue
      sadd sheetRangesKey [rangeKey']
      return ()

-- | Takes in a cell that's tied to a list. Decouples all the cells in that list from that
-- | returns: cells before decoupling
-- Note: this operation is O(n)
-- TODO move to C client because it's expensive
decouple :: Connection -> RangeKey -> IO [ASCell]
decouple conn key = 
  let rangeKey = B.pack key
      sheetRangesKey = DU.getSheetRangesKey $ DU.rangeKeyToSheetId key
  in do
    locKeys <- runRedis conn $ do
      TxSuccess result <- multiExec $ do
        result <- smembers rangeKey
        del [rangeKey]
        srem sheetRangesKey [rangeKey]
        return result
      return result
    printWithTime $ "got coupled locs: " ++ (U.truncated $ show locKeys)
    case locKeys of
      [] -> return []
      ls -> U.filterNothing <$> DU.getCellsByKeys ls

--getFatCellsInRange :: Connection -> ASRange -> IO [RangeKey]
--getFatCellsInRange conn rng = 


----------------------------------------------------------------------------------------------------------------------
-- helpers

-- | Returns the listkeys of all the lists that are entirely contained in the range.  
getFatCellsInRange :: Connection -> ASRange -> IO [RangeKey]
getFatCellsInRange conn rng = do
  let sid = rangeSheetId rng
  rangeKeys <- DU.getRangeKeysInSheet conn sid
  let rects = map DU.rangeKeyToRect rangeKeys
      zipRects = zip rangeKeys rects
      zipRectsContained = filter (\(_,rect) -> U.rangeContainsRect rng rect) zipRects
  return $ map fst zipRectsContained


--recoupleList :: Connection -> RangeDescriptor -> IO ()
--recoupleList conn key = do
--  let locs = DU.getLocationsFromListKey key
--  locsExist <- locationsExist conn locs
--  let locs' = U.zipFilter $ zip locs locsExist
--  setListLocations conn key locs'

  -- | Makes sure everything is synced -- the listKeys and ancestors in graph db should reflect 
-- the cell changes that happen as a result of setting the cells. 
setCellsPropagated :: Connection -> [ASCell] -> IO ()
setCellsPropagated conn cells = 
  let rangeKeys = nub $ catMaybes $ map DU.cellToRangeKey cells
      roots     = filter (\c -> (not $ DU.isFatCellMember c) || DU.isFatCellHead c) cells
  in do
    setCells cells
    setCellsAncestorsForce roots
    mapM_ (decouple conn) rangeKeys

-- | Makes sure everything is synced -- the listKeys and ancestors in graph db should reflect 
-- the cell changes that happen as a result of deleting the cells. 
deleteCellsPropagated :: Connection -> [ASCell] -> IO ()
deleteCellsPropagated conn cells = 
  let rangeKeys = nub $ catMaybes $ map DU.cellToRangeKey cells
  in do 
    deleteCells conn cells
    setCellsAncestorsForce . U.blankCellsAt $ map cellLocation cells
    mapM_ (decouple conn) rangeKeys

----------------------------------------------------------------------------------------------------------------------
-- Commits

-- TODO: need to deal with large commit sizes and max number of commits
-- TODO: need to delete blank cells from the DB. (Otherwise e.g. if you delete a
-- a huge range, you're going to have all those cells in the DB doing nothing.)

-- | Creates and pushes a commit to the DB
addCommit :: Connection -> ([ASCell], [ASCell]) -> CommitSource -> IO ()
addCommit conn (b,a) src = do
  time <- getASTime
  let commit = ASCommit b a time
  pushCommit conn commit src
  --putStrLn $ show commit

pushKey :: CommitSource -> B.ByteString
pushKey (sid, uid) = B.pack $ (T.unpack sid) ++ '|':(T.unpack uid) ++ "pushed"

popKey :: CommitSource -> B.ByteString
popKey (sid, uid)  = B.pack $ (T.unpack sid) ++ '|':(T.unpack uid) ++ "popped"

-- | Return a commit if possible (not possible if you undo past the beginning of time, etc)
-- | Update the DB so that there's always a source of truth (ie we will initEval undo to all relevant users)
undo :: Connection -> CommitSource -> IO (Maybe ASCommit)
undo conn src = do
  commit <- runRedis conn $ do
    TxSuccess justC <- multiExec $ do
      commit <- rpoplpush (pushKey src) (popKey src)
      return commit
    return $ DU.bStrToASCommit justC
  case commit of
    Nothing -> return Nothing
    Just c@(ASCommit b a t) -> do
      deleteCellsPropagated conn a
      setCellsPropagated conn b
      -- mapM_ (recoupleList conn) listKeys
      return $ Just c

redo :: Connection -> CommitSource -> IO (Maybe ASCommit)
redo conn src = do
  commit <- runRedis conn $ do
    Right result <- lpop (popKey src)
    case result of
      (Just commit) -> do
        rpush (pushKey src) [commit]
        return $ DU.bStrToASCommit (Just commit)
      _ -> return Nothing
  case commit of
    Nothing -> return Nothing
    Just c@(ASCommit b a t) -> do
      deleteCellsPropagated conn b
      setCellsPropagated conn a
      -- mapM_ (decoupleList conn) listKeys
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
