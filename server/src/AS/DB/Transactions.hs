module AS.DB.Transactions where

import AS.Types.Core
import AS.Types.DB
import AS.DB.API as DB
import AS.DB.Util as DU

----------------------------------------------------------------------------------------------------------------------
-- top-level functions

-- | Deal with updating all DB-related things after an eval. 
updateAfterEval :: Connection -> ASTransaction -> EitherTExec [ASCell]
updateAfterEval conn (Transaction src@(sid, _) roots afterCells fatCells) = do
  let newListCells         = concat $ map snd lists
      afterCellsWithLists  = afterCells ++ newListCells
      cellLocs             = map cellLocation afterCellsWithLists
  beforeCells     <- lift $ catMaybes <$> getCells cellLocs
  listKeysChanged <- liftIO $ DU.getListIntersections conn sid cellLocs
  decoupleResult  <- lift $ mapM (decoupleList conn) listKeysChanged
  let (coupledCells, decoupledCells) = U.liftListTuple decoupleResult
      afterCells'                    = U.mergeCells afterCellsWithLists decoupledCells
      beforeCells'                   = U.mergeCells beforeCells coupledCells
  liftIO $ setCells afterCells'
  liftIO $ deleteCells conn (filter isEmptyCell afterCells')
  liftIO $ mapM_ (\(key, cells) -> setListLocations conn key (map cellLocation cells)) lists
  liftIO $ addCommit conn (beforeCells', afterCells') src
  liftIO $ printWithTime "added commits"
  right afterCells'

----------------------------------------------------------------------------------------------------------------------
-- helpers

couple :: Connection -> FatCell -> IO ()
couple conn desc = 

decouple :: Connection -> RangeKey -> IO [ASCell]
decouple conn key = 

getFatCellsInRange :: Connection -> ASRange -> IO [RangeKey]
getFatCellsInRange conn rng = 


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

setListLocations :: Connection -> ListKey -> [ASIndex] -> IO ()
setListLocations conn listKey locs
  | null locs = return ()
  | otherwise = do 
    let locKeys       = map DU.getLocationKey locs
        listKey'      = B.pack listKey
        sheetListsKey = DU.getSheetListsKey . locSheetId $ head locs
    runRedis conn $ do
      liftIO $ printWithTime $ "setting list locations for key: " ++ listKey
      sadd listKey' locKeys
      sadd sheetListsKey [listKey']
      return ()

-- | Takes in a cell that's tied to a list. Decouples all the cells in that list from that
-- | returns: (cells before decoupling, cells after decoupling)
-- Note: this operation is O(n)
-- TODO move to C client because it's expensive
decoupleList :: Connection -> ListKey -> IO ([ASCell], [ASCell])
decoupleList conn listString = do
  let listKey = B.pack listString
  let sheetListsKey = DU.getSheetListsKey $ DU.getSheetIdFromListKey listString
  locs <- runRedis conn $ do
    TxSuccess result <- multiExec $ do
      result <- smembers listKey
      del [listKey]
      srem sheetListsKey [listKey]
      return result
    return result
  printWithTime $ "got coupled locs: " ++ (U.truncated $ show locs)
  case locs of
    [] -> return ([],[])
    ls -> do
      listCells <- U.filterNothing <$> DU.getCellsByKeys ls
      let decoupledCells = map DU.decoupleCell listCells
      return (listCells, decoupledCells)

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

pushKey :: CommitSource -> BS.ByteString
pushKey (sid, uid) = B.pack $ (T.unpack sid) ++ '|':(T.unpack uid) ++ "pushed"

popKey :: CommitSource -> BS.ByteString
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
