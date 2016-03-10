module AS.DB.Expanding where

import Prelude()
import AS.Prelude

import AS.Types.Cell
import AS.Types.Eval
import AS.Types.DB
import AS.DB.API as DB
import AS.DB.Internal as DI
import AS.Logging
import qualified AS.Serialize as S

import Data.List (nub)
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B

import Database.Redis hiding (time)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Control.Concurrent

-------------------------------------------------------------------------------------------------------------------------
-- This module is for database functions associated with expanding values.

-------------------------------------------------------------------------------------------------------------------------
-- coupling

setDescriptor :: Connection -> RangeDescriptor -> IO ()
setDescriptor conn descriptor = 
  let rangeKey        = descriptorKey descriptor 
      rangeKey'       = toRedisFormat . RedisRangeKey $ rangeKey
      sheetRangesKey  = toRedisFormat . SheetRangesKey . rangeKeyToSheetId $ rangeKey
  in runRedis conn $ do
      set rangeKey' (S.encode descriptor)
      sadd sheetRangesKey [rangeKey']
      return ()

deleteDescriptor :: Connection -> RangeKey -> IO ()
deleteDescriptor conn rangeKey = 
  let rangeKey'      = toRedisFormat . RedisRangeKey $ rangeKey
      sheetRangesKey = toRedisFormat . SheetRangesKey . rangeKeyToSheetId $ rangeKey
  in runRedis conn $ do
    del [rangeKey']
    srem sheetRangesKey [rangeKey']
    return ()

-------------------------------------------------------------------------------------------------------------------------
-- Range keys

-- Seems obsolete now. (--Alex 1/3/16)
-- | Returns the listkeys of all the lists that are entirely contained in the range.  
-- getFatCellsInRange :: Connection -> ASRange -> IO [RangeKey]
-- getFatCellsInRange conn rng = do
--   let sid = rangeSheetId rng
--   rangeKeys <- DB.getRangeKeysInSheet conn sid
--   let rects = map rangeRect rangeKeys
--       zipRects = zip rangeKeys rects
--       zipRectsContained = filter (\(_, rect) -> rangeContainsRect rng rect) zipRects
--   return $ map fst zipRectsContained
