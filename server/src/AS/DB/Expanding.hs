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

setDescriptor :: Connection -> RangeDescriptor -> IO ()
setDescriptor conn d = 
  let rangeKey        = descriptorKey d 
      rangeKey'       = B.pack . show2 $ rangeKey
      sheetRangesKey  = makeSheetRangesKey . rangeKeyToSheetId $ rangeKey
      rangeDescriptor = B.pack $ show d
  in runRedis conn $ do
      liftIO $ printWithTime $ "setting list locations for key: " ++ (show2 rangeKey)
      set rangeKey' rangeDescriptor
      sadd sheetRangesKey [rangeKey']
      return ()

deleteDescriptor :: Connection -> RangeDescriptor -> IO ()
deleteDescriptor conn d = 
  let key            = descriptorKey d
      key'           = B.pack . show2 $ key
      sheetRangesKey = makeSheetRangesKey . rangeKeyToSheetId $ key
  in runRedis conn $ do
    del [key']
    srem sheetRangesKey [key']
    return ()

-- | Takes in a cell that's tied to a list. Decouples all the cells in that list from that
-- | r
-- turns: cells before decoupling
-- Note: this operation is O(n)
-- TODO move to C client because it's expensive
--decouple :: Connection -> RangeKey -> IO [ASCell]
--decouple conn key = 
--  let rangeKey       = B.pack . show2 $ key
--      sheetRangesKey = makeSheetRangesKey . rangeKeyToSheetId $ key
--  in do
--    runRedis conn $ multiExec $ do
--      del [rangeKey]
--      srem sheetRangesKey [rangeKey]
--    catMaybes <$> DB.getCells (rangeKeyToIndices key)

-- Same as above, but don't modify DB (we want to send a decoupling warning)
-- Still gets the cells before decoupling, but don't set range keys
getCellsBeforeDecoupling :: Connection -> RangeKey -> IO [ASCell]
getCellsBeforeDecoupling conn key = catMaybes <$> DB.getCells (rangeKeyToIndices key)

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

getFatCellIntersections :: Connection -> EvalContext -> Either [ASIndex] [RangeKey] -> IO [RangeDescriptor]
getFatCellIntersections conn ctx (Left locs) = (filter descriptorIntersects) . concat <$> mapM (getRangeDescriptorsInSheetWithContext conn ctx) sheetIds
  where
    sheetIds = L.nub $ map locSheetId locs
    descriptorIntersects r = anyLocsContainedInRect locs (rangeRect . descriptorKey $ r)
    anyLocsContainedInRect ls r = any id $ map (indexInRect r) ls
    indexInRect ((a',b'),(a2',b2')) (Index _ (a,b)) = a >= a' && b >= b' &&  a <= a2' && b <= b2'

getFatCellIntersections conn ctx (Right keys) = do
  descriptors <- concat <$> mapM (getRangeDescriptorsInSheetWithContext conn ctx) sheetIds
  printObj "Checking intersections against keys in sheet" descriptors
  return $ descriptorsIntersectingKeys descriptors keys
    where 
      sheetIds = L.nub $ map (locSheetId . keyIndex) keys
      -- given a list of keys and a descriptor, return True iff the descriptor intersects any of the keys
      descriptorIntersectsAnyKeyInList ks d = length (filter (\key -> keysIntersect (descriptorKey d) key) ks) > 0
      descriptorsIntersectingKeys ds ks = filter (descriptorIntersectsAnyKeyInList ks) ds
      keysIntersect k1 k2    = rectsIntersect (rangeRect k1) (rangeRect k2)
      rectsIntersect ((y,x),(y2,x2)) ((y',x'),(y2',x2'))
        | y2 < y' = False 
        | y > y2' = False
        | x2 < x' = False 
        | x > x2' = False
        | otherwise = True 