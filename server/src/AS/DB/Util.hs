module AS.DB.Util where

import Prelude

import AS.Types 
import AS.Util
import AS.Parsing.Common (tryParseListNonIso)
import AS.Parsing.In (int)

import qualified Data.List as L

import qualified Data.Text as T
import Database.Redis hiding (decode, Message)
import Data.List.Split

import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString               as B
import qualified Text.Show.ByteString          as BS
import qualified Data.ByteString.Internal      as BI
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import           Foreign.ForeignPtr
import           Foreign.Ptr

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans

----------------------------------------------------------------------------------------------------------------------
-- | Settings

dagChunkSize :: Int
dagChunkSize = 1000

-- | Haskell Redis connection object
cInfo :: ConnectInfo
cInfo = ConnInfo
    { connectHost           = "localhost"
    , connectPort           = PortNumber 6379
    , connectAuth           = Nothing
    , connectDatabase       = 0               
    , connectMaxConnections = 100
    , connectMaxIdleTime    = 1000000
    }


----------------------------------------------------------------------------------------------------------------------
-- | Redis key utilities

getLocationKey :: ASLocation -> B.ByteString
getLocationKey = pack2 . show2

getSheetKey :: ASSheetId -> B.ByteString -- for storing the actual sheet as key-value
getSheetKey = pack2 . T.unpack 

getSheetSetKey :: ASSheetId -> B.ByteString -- for storing set of locations in single sheet
getSheetSetKey sid = pack2 $! (T.unpack sid) ++ "Locations"

getWorkbookKey :: String -> B.ByteString
getWorkbookKey = pack2

keyToRow :: B.ByteString -> Int
keyToRow str = row
  where
    (col, row) = read idxStr :: (Int, Int)
    idxStr = BC.unpack $ last $ BC.split '|' str

getLastRowKey :: [B.ByteString] -> B.ByteString
getLastRowKey keys = maxBy keyToRow keys

incrementLocKey :: (Int, Int) -> B.ByteString -> B.ByteString
incrementLocKey (dx, dy) key = pack2 $ ks ++ '|':kidx
  where
    (sh:idxStr:[]) = BC.split '|' key
    ks = BC.unpack sh
    (col, row) = read (BC.unpack idxStr) :: (Int, Int)
    kidx = show (col + dx, row + dy)

getUniquePrefixedName :: String -> [String] -> String
getUniquePrefixedName pref strs = pref ++ (show idx)
  where
    strs' = filter (L.isPrefixOf pref) strs
    strs'' = map (drop . length $ pref) strs'
    idxs = tryParseListNonIso int strs''
    idx = case idxs of 
      [] -> 1
      _ -> (L.maximum idxs) + 1

----------------------------------------------------------------------------------------------------------------------
-- | private DB functions

getCellByKeyRedis :: B.ByteString -> Redis (Maybe ASCell)
getCellByKeyRedis key = do
    Right str <- get key
    return $ bStrToASCell str

setCellRedis :: ASCell -> Redis ()
setCellRedis cell = do
    let loc = cellLocation cell
        key = getLocationKey loc
        cellstr = pack2 . show2 $ cell
    _ <- set key cellstr
    let setKey = getSheetSetKey (locSheetId loc)
    _ <- sadd setKey [key] -- add the location key to the set of locs in a sheet (for sheet deletion etc)
    return ()

deleteLocRedis :: ASLocation -> Redis ()
deleteLocRedis loc = del [getLocationKey loc] >> return ()

getSheetLocsRedis :: ASSheetId -> Redis [B.ByteString]
getSheetLocsRedis sheetid = do
  Right keys <- smembers $ getSheetSetKey sheetid
  return keys

----------------------------------------------------------------------------------------------------------------------
-- | ByteString utils

toStrict2 :: BL.ByteString -> B.ByteString
toStrict2 BLI.Empty = B.empty
toStrict2 (BLI.Chunk c BLI.Empty) = c
toStrict2 lb = BI.unsafeCreate len $ go lb
  where
    len = BLI.foldlChunks (\l sb -> l + B.length sb) 0 lb
    go  BLI.Empty                   _   = return ()
    go (BLI.Chunk (BI.PS fp s l) r) ptr =
        withForeignPtr fp $ \p -> do
            BI.memcpy ptr (p `plusPtr` s) (fromIntegral l)
            go r (ptr `plusPtr` l)

pack2 :: String -> B.ByteString
pack2 str = toStrict2 . BS.show $! str

----------------------------------------------------------------------------------------------------------------------
-- | Reading bytestrings

bStrToASExpression :: Maybe B.ByteString -> Maybe ASExpression
bStrToASExpression (Just b) = Just (read2 (BC.unpack b) :: ASExpression)
bStrToASExpression Nothing = Nothing

bStrToASValue :: Maybe B.ByteString -> Maybe ASValue
bStrToASValue (Just b) = Just (read2 (BC.unpack b) :: ASValue)
bStrToASValue Nothing = Nothing     

bStrToTags :: Maybe B.ByteString -> Maybe [ASCellTag]
bStrToTags (Just b) = Just (read (BC.unpack b) :: [ASCellTag])
bStrToTags Nothing = Nothing 

maybeASCell :: (ASLocation,Maybe ASExpression,Maybe ASValue, Maybe [ASCellTag]) -> Maybe ASCell
maybeASCell (l, Just e, Just v, Just tags) = Just $ Cell l e v tags
maybeASCell _ = Nothing

bStrToASLocation :: B.ByteString -> ASLocation
bStrToASLocation b = (read2 (BC.unpack b) :: ASLocation)

bStrToASCommit :: Maybe B.ByteString -> Maybe ASCommit 
bStrToASCommit (Just b) = Just (read (BC.unpack b) :: ASCommit)
bStrToASCommit Nothing = Nothing

bStrToSheet :: Maybe B.ByteString -> Maybe ASSheet
bStrToSheet (Just b) = Just (read (BC.unpack b) :: ASSheet)
bStrToSheet Nothing = Nothing

bStrToWorkbook :: Maybe B.ByteString -> Maybe ASWorkbook
bStrToWorkbook (Just b) = Just (read (BC.unpack b) :: ASWorkbook)
bStrToWorkbook Nothing = Nothing

bStrToASCell :: Maybe B.ByteString -> Maybe ASCell
bStrToASCell Nothing = Nothing
bStrToASCell (Just str) = Just (read2 (BC.unpack str) :: ASCell)