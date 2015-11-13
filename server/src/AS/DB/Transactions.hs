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
