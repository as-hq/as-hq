module AS.DB.Transaction where

import Prelude

import AS.Types.Cell
import AS.Types.DB
import AS.DB.API as DB
import AS.DB.Util as DU
import AS.Logging

import Database.Redis
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Data.List (nub)
import Data.Maybe (catMaybes, fromJust)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class

----------------------------------------------------------------------------------------------------------------------
-- top-level functions


-- takes in [new cells], [new fat cells] as a result of evalChain, and returns the list 
-- of locations decoupled as a result.
getDecouplingEffects :: Connection -> ASSheetId -> [ASCell] -> [FatCell] -> IO ([ASIndex])
getDecouplingEffects conn sid cells fcells = 
  let locs = map cellLocation cells
      keys = map (descriptorKey . descriptor) fcells
  in do
    rangeKeysChangedByCells    <- liftIO $ DU.getFatCellIntersections conn sid (Left locs)
    rangeKeysChangedByFatCells <- liftIO $ DU.getFatCellIntersections conn sid (Right keys)
    let rangeKeysChanged = rangeKeysChangedByCells ++ rangeKeysChangedByFatCells
    let decoupledLocs    = concat $ map DU.rangeKeyToIndices rangeKeysChanged
    return decoupledLocs

-- | Deal with updating all DB-related things after an eval. 
writeTransaction :: Connection -> ASTransaction -> EitherTExec [ASCell]
writeTransaction conn (Transaction src@(sid, _) afterCells afterDescriptors deletedLocs) = 
  let deletedCells     = blankCellsAt deletedLocs
      afterCells'      = mergeCells afterCells deletedCells
      locs'            = map cellLocation afterCells'
      rangeKeys        = map descriptorKey afterDescriptors
  in do
    printWithTimeT $ "GOT DELETED LOCS: " ++ (show $ map show2 deletedLocs)
    beforeCells <- lift $ catMaybes <$> DB.getCells locs'
    -- determine all fatcell intersections produced by eval
    rangeKeysChanged <- liftIO $ DU.getFatCellIntersections conn sid (Left locs')
    -- hold on to the decoupled descriptors 
    beforeDescriptors <- liftIO $ map fromJust <$> mapM (DB.getRangeDescriptor conn) rangeKeysChanged
    printWithTimeT $ "Range keys changed: " ++ (show rangeKeysChanged)
    -- decouple the intersected fat cells
    coupledCells <- lift $ concat <$> (mapM (decouple conn) rangeKeysChanged)
    let decoupledCells  = map DU.decoupleCell coupledCells
        afterCells''    = mergeCells afterCells' decoupledCells
        beforeCells'    = mergeCells beforeCells coupledCells
    -- set the database
    liftIO $ DB.setCells afterCells''
    -- delete empty cells after setting them
    liftIO $ deleteCells conn $ filter isEmptyCell afterCells''
    liftIO $ mapM_ (couple conn) afterDescriptors
    --construct commit
    time <- lift $ getASTime
    let commit = Commit beforeCells' afterCells'' beforeDescriptors afterDescriptors time
    lift $ pushCommit conn commit src
    right afterCells''

----------------------------------------------------------------------------------------------------------------------
-- helpers

couple :: Connection -> RangeDescriptor -> IO ()
couple conn desc = 
  let rangeKey        = descriptorKey desc 
      rangeKey'       = B.pack rangeKey 
      sheetRangesKey  = DU.makeSheetRangesKey $ DU.rangeKeyToSheetId rangeKey
      rangeDescriptor = B.pack $ show desc
  in runRedis conn $ do
      liftIO $ printWithTime $ "setting list locations for key: " ++ rangeKey
      set rangeKey' rangeDescriptor
      sadd sheetRangesKey [rangeKey']
      return ()

-- | Takes in a cell that's tied to a list. Decouples all the cells in that list from that
-- | returns: cells before decoupling
-- Note: this operation is O(n)
-- TODO move to C client because it's expensive
decouple :: Connection -> RangeKey -> IO [ASCell]
decouple conn key = 
  let rangeKey = B.pack key
      sheetRangesKey = DU.makeSheetRangesKey $ DU.rangeKeyToSheetId key
  in do
    runRedis conn $ multiExec $ do
      del [rangeKey]
      srem sheetRangesKey [rangeKey]
    catMaybes <$> DB.getCells (DU.rangeKeyToIndices key)

----------------------------------------------------------------------------------------------------------------------
-- helpers

-- | Returns the listkeys of all the lists that are entirely contained in the range.  
getFatCellsInRange :: Connection -> ASRange -> IO [RangeKey]
getFatCellsInRange conn rng = do
  let sid = rangeSheetId rng
  rangeKeys <- DU.makeRangeKeysInSheet conn sid
  let rects = map DU.rangeRect rangeKeys
      zipRects = zip rangeKeys rects
      zipRectsContained = filter (\(_, rect) -> rangeContainsRect rng rect) zipRects
  return $ map fst zipRectsContained

  -- | Makes sure everything is synced -- the listKeys and ancestors in graph db should reflect 
-- the cell changes that happen as a result of setting the cells. 
setCellsPropagated :: Connection -> [ASCell] -> [RangeDescriptor] -> IO ()
setCellsPropagated conn cells descs = 
  let roots = filter (\c -> (not $ DU.isFatCellMember c) || DU.isFatCellHead c) cells
  in do
    setCells cells
    setCellsAncestorsForce roots
    mapM_ (couple conn) descs

-- | Makes sure everything is synced -- the listKeys and ancestors in graph db should reflect 
-- the cell changes that happen as a result of deleting the cells. 
deleteCellsPropagated :: Connection -> [ASCell] -> [RangeDescriptor] -> IO ()
deleteCellsPropagated conn cells descs = do
  deleteCells conn cells
  removeAncestorsAtForced $ map cellLocation cells
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
    return $ DU.bStrToASCommit commit
  case commit of
    Nothing -> return Nothing
    Just c@(Commit b a bd ad t) -> do
      deleteCellsPropagated conn a ad
      setCellsPropagated conn b bd
      return $ Just c

redo :: Connection -> CommitSource -> IO (Maybe ASCommit)
redo conn src = do
  commit <- runRedis conn $ do
    Right result <- lpop (popKey src)
    case result of
      Just commit -> do
        rpush (pushKey src) [commit]
        return $ DU.bStrToASCommit (Just commit)
      _ -> return Nothing
  case commit of
    Nothing -> return Nothing
    Just c@(Commit b a bd ad t) -> do
      deleteCellsPropagated conn b bd
      setCellsPropagated conn a ad
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
