module AS.DB.Transaction where

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

----------------------------------------------------------------------------------------------------------------------
-- top-level functions

-- | Deal with updating all DB-related things after an eval. 
writeTransaction :: Connection -> ASTransaction -> EitherTExec [ASCell]
writeTransaction conn (Transaction src@(sid, _) roots afterCells fatCells) = 
  let expandedCells   = concat $ map (\(FatCell cs _ _) -> cs) fatCells
      locs            = map cellLocation afterCells
      afterCells'     = afterCells ++ expandedCells
      locs'           = map cellLocation afterCells'
      rangeKeys       = map (\(FatCell _ _ desc) -> getRangeKey desc) fatCells
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
    liftIO $ setCells $ filter (not . isEmptyCell) afterCells''
    liftIO $ mapM_ (\(FatCell _ desc) -> couple conn) fatCells
    liftIO $ addCommit conn (beforeCells', afterCells') src
    liftIO $ printWithTime "added commits"
    right afterCells'

----------------------------------------------------------------------------------------------------------------------
-- helpers

couple :: Connection -> RangeDescriptor -> IO ()
couple conn desc = 
  let rangeKey       = DU.getRangeKey desc 
      rangeKey'      = B.pack rangeKey
      sheetRangesKey = DU.getSheetRangesKey $ DU.rangeKeyToSheetId rangeKey
      rangeValue     = B.pack $ show desc
  in runRedis conn $ do
      liftIO $ printWithTime $ "setting list locations for key: " ++ listKey
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
        result <- smembers listKey
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
getListsInRange :: Connection -> ASRange -> IO [ListKey]
getListsInRange conn rng = do
  let sid = rangeSheetId rng
  listKeys <- DU.getListKeysInSheet conn sid
  let rects = map DU.getRectFromListKey listKeys
      zipRects = zip listKeys rects
      zipRectsContained = filter (\(_,rect) -> U.rangeContainsRect rng rect) zipRects
  return $ map fst zipRectsContained


recoupleList :: Connection -> ListKey -> IO ()
recoupleList conn key = do
  let locs = DU.getLocationsFromListKey key
  locsExist <- locationsExist conn locs
  let locs' = U.zipFilter $ zip locs locsExist
  setListLocations conn key locs'

  -- | Makes sure everything is synced -- the listKeys and ancestors in graph db should reflect 
-- the cell changes that happen as a result of setting the cells. 
setCellsPropagated :: Connection -> [ASCell] -> IO ()
setCellsPropagated conn cells = do 
  setCells cells
  setCellsAncestorsForce $ filter (\c -> (not $ U.isListMember c) || DU.isListHead c) cells
  let listKeys = nub $ catMaybes $ map DU.getCellListKey cells
  mapM_ (recoupleList conn) listKeys

-- | Makes sure everything is synced -- the listKeys and ancestors in graph db should reflect 
-- the cell changes that happen as a result of deleting the cells. 
deleteCellsPropagated :: Connection -> [ASCell] -> IO ()
deleteCellsPropagated conn cells = do 
  deleteCells conn cells
  setCellsAncestorsForce $ U.blankCellsAt (map cellLocation cells)
  let listKeys = nub $ catMaybes $ map DU.getCellListKey cells
  mapM_ (decoupleList conn) listKeys

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
