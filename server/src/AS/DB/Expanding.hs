module AS.DB.Expanding where

import Prelude

import AS.Types.Cell
import AS.Types.Eval
import AS.Types.DB
import AS.DB.API as DB
import AS.DB.Internal as DI
import AS.Logging

import Database.Redis hiding (time)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import Data.List (nub)
import Data.Maybe (catMaybes, fromJust)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Control.Concurrent

-------------------------------------------------------------------------------------------------------------------------
-- This module is for database functions associated with expanding values.

-------------------------------------------------------------------------------------------------------------------------
-- coupling

couple :: Connection -> RangeDescriptor -> IO ()
couple conn desc = 
  let rangeKey        = descriptorKey desc 
      rangeKey'       = B.pack . show2 $ rangeKey
      sheetRangesKey  = makeSheetRangesKey . rangeKeyToSheetId $ rangeKey
      rangeDescriptor = B.pack $ show desc
  in runRedis conn $ do
      liftIO $ printWithTime $ "setting list locations for key: " ++ (show2 rangeKey)
      set rangeKey' rangeDescriptor
      sadd sheetRangesKey [rangeKey']
      return ()

-- | Takes in a cell that's tied to a list. Decouples all the cells in that list from that
-- | returns: cells before decoupling
-- Note: this operation is O(n)
-- TODO move to C client because it's expensive
decouple :: Connection -> RangeKey -> IO [ASCell]
decouple conn key = 
  let rangeKey       = B.pack . show2 $ key
      sheetRangesKey = makeSheetRangesKey . rangeKeyToSheetId $ key
  in do
    runRedis conn $ multiExec $ do
      del [rangeKey]
      srem sheetRangesKey [rangeKey]
    catMaybes <$> DB.getCells (rangeKeyToIndices key)

-- Same as above, but don't modify DB (we want to send a decoupling warning)
-- Still gets the cells before decoupling, but don't set range keys
getCellsBeforeDecoupling :: Connection -> RangeKey -> IO [ASCell]
getCellsBeforeDecoupling conn key = catMaybes <$> DB.getCells (rangeKeyToIndices key)
  where 
    rangeKey       = B.pack . show2 $ key
    sheetRangesKey = makeSheetRangesKey . rangeKeyToSheetId $ key


-------------------------------------------------------------------------------------------------------------------------
-- Range keys

-- | Returns the listkeys of all the lists that are entirely contained in the range.  
getFatCellsInRange :: Connection -> ASRange -> IO [RangeKey]
getFatCellsInRange conn rng = do
  let sid = rangeSheetId rng
  rangeKeys <- DI.getRangeKeysInSheet conn sid
  let rects = map rangeRect rangeKeys
      zipRects = zip rangeKeys rects
      zipRectsContained = filter (\(_, rect) -> rangeContainsRect rng rect) zipRects
  return $ map fst zipRectsContained

-- We also want to store the changed range keys in the DB, so that we can actually do the decoupling
-- (remove range key etc) if user says OK
getRangeKeysChanged :: Connection -> CommitSource -> IO (Maybe [RangeKey])
getRangeKeysChanged conn src = do 
  let commitSource = B.pack $ (show src) ++ "rangekeys"
  maybeRKeys <- runRedis conn $ do
    TxSuccess rkeys <- multiExec $ do
      get commitSource 
    return rkeys
  return $ bStrToRangeKeys maybeRKeys

setRangeKeysChanged :: Connection  -> [RangeKey] -> CommitSource -> IO ()
setRangeKeysChanged conn keys src = do 
  let rangeKeys = (B.pack . show) keys 
  let commitSource = B.pack $ (show src) ++ "rangekeys"
  runRedis conn $ do
    TxSuccess _ <- multiExec $ do
      set commitSource rangeKeys
    return ()

-- given a list of rangeKeys, remove the ones that we've indicated as removed in the given evalContext
filterRangeKeysByContext :: EvalContext -> [RangeKey] -> [RangeKey]
filterRangeKeysByContext (EvalContext _ _ removedDescriptors _) = filter (not . isRemoved)
  where 
    removedRangeKeys = map descriptorKey removedDescriptors
    isRemoved        = flip elem removedRangeKeys

getFatCellIntersections :: Connection -> EvalContext -> Either [ASIndex] [RangeKey] -> IO [RangeKey]
getFatCellIntersections conn ctx (Left locs) = (filterKeys ctx) . concat <$> getRangeKeys conn locs
  where
    filterKeys ctx              = (filter keyIntersects) . (filterRangeKeysByContext ctx)
    getRangeKeys conn myLocs    = mapM (DI.getRangeKeysInSheet conn) (L.nub $ map locSheetId myLocs)
    keyIntersects k             = anyLocsContainedInRect locs (rangeRect k)
    anyLocsContainedInRect ls r = any id $ map (indexInRect r) ls
    indexInRect ((a',b'),(a2',b2')) (Index _ (a,b)) = a >= a' && b >= b' &&  a <= a2' && b <= b2'

getFatCellIntersections conn ctx (Right keys) = do
  rangeKeys <- concat <$> getRangeKeys conn keys
  printObj "Checking intersections against keys in sheet" rangeKeys
  return $ filterKeys ctx rangeKeys 
    where 
      getRangeKeys conn myKeys = mapM (getRangeKeysInSheet conn) (L.nub $ map (locSheetId . keyIndex) myKeys)
      filterKeys ctx         = filterIntersectingKeys . (filterRangeKeysByContext ctx)
      filterIntersectingKeys = L.intersectBy keysIntersect keys
      keysIntersect k1 k2    = rectsIntersect (rangeRect k1) (rangeRect k2)
      rectsIntersect ((y,x),(y2,x2)) ((y',x'),(y2',x2'))
        | y2 < y' = False 
        | y > y2' = False
        | x2 < x' = False 
        | x > x2' = False
        | otherwise = True 
