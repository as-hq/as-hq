module AS.DB.Expanding where

import Prelude

import AS.Types.Cell
import AS.Types.DB
import AS.DB.API as DB
import AS.DB.Util as DU
import AS.Logging

import Database.Redis hiding (time)
import qualified Data.Text as T
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
      sheetRangesKey  = DU.makeSheetRangesKey $ DU.rangeKeyToSheetId rangeKey
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
      sheetRangesKey = DU.makeSheetRangesKey $ DU.rangeKeyToSheetId key
  in do
    runRedis conn $ multiExec $ do
      del [rangeKey]
      srem sheetRangesKey [rangeKey]
    catMaybes <$> DB.getCells (DU.rangeKeyToIndices key)

-- Same as above, but don't modify DB (we want to send a decoupling warning)
-- Still gets the cells before decoupling, but don't set range keys
getCellsBeforeDecoupling :: Connection -> RangeKey -> IO [ASCell]
getCellsBeforeDecoupling conn key = catMaybes <$> DB.getCells (DU.rangeKeyToIndices key)
  where 
    rangeKey       = B.pack . show2 $ key
    sheetRangesKey = DU.makeSheetRangesKey $ DU.rangeKeyToSheetId key


-------------------------------------------------------------------------------------------------------------------------
-- Range keys

-- | Returns the listkeys of all the lists that are entirely contained in the range.  
getFatCellsInRange :: Connection -> ASRange -> IO [RangeKey]
getFatCellsInRange conn rng = do
  let sid = rangeSheetId rng
  rangeKeys <- DU.getRangeKeysInSheet conn sid
  let rects = map DU.rangeRect rangeKeys
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

