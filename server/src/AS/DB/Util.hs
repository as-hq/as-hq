module AS.DB.Util where

import Prelude

import AS.Types.DB
import AS.Types.Cell
import AS.Types.CellProps
import AS.Types.Eval

import AS.Util as U
import AS.Logging
import AS.Parsing.Common (tryParseListNonIso)
import AS.Parsing.Read (integer)
import AS.Parsing.Show (showPrimitive)

import qualified Data.List                     as L
import qualified Data.Text                     as T
import           Data.List.Split
import Data.Word (Word8)
import Data.Maybe (fromJust, catMaybes)

import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString               as B
import qualified Text.Show.ByteString          as BS
import qualified Data.ByteString.Internal      as BI
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.ByteString.Unsafe        as BU
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.C.String(CString, peekCString)

import Database.Redis hiding (decode, Message)

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans

import Foreign
import Foreign.C.Types
import Foreign.C.String(CString(..))
import Foreign.C

----------------------------------------------------------------------------------------------------------------------
-- Settings

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
-- Redis key utilities

-- key for set of all fat cells in a sheet
makeSheetRangesKey :: ASSheetId -> B.ByteString
makeSheetRangesKey sid = BC.pack $ (T.unpack sid) ++ (keyPartDelimiter:"ALL_RANGES")

-- key for locations
makeLocationKey :: ASIndex -> B.ByteString
makeLocationKey = BC.pack . show2

-- key for sheet
makeSheetKey :: ASSheetId -> B.ByteString -- for storing the actual sheet as key-value
makeSheetKey = BC.pack . T.unpack

-- key for all location keys in a sheet
makeSheetSetKey :: ASSheetId -> B.ByteString
makeSheetSetKey sid = BC.pack $! (T.unpack sid) ++ "Locations"

-- key for workbook
makeWorkbookKey :: String -> B.ByteString
makeWorkbookKey = BC.pack

-- given "Untitled" and ["Untitled1", "Untitled2"], produces "Untitled3"
-- only works on integer suffixes
getUniquePrefixedName :: String -> [String] -> String
getUniquePrefixedName pref strs = pref ++ (show idx)
  where
    strs'   = filter (L.isPrefixOf pref) strs
    strs''  = map (drop . length $ pref) strs'
    idxs    = tryParseListNonIso integer strs''
    idx     = case idxs of
      [] -> 1
      _  -> (L.maximum idxs) + 1

----------------------------------------------------------------------------------------------------------------------
-- FFI

foreign import ccall unsafe "hiredis/redis_db.c getCells" c_getCells :: CString -> CInt -> IO (Ptr CString)
foreign import ccall unsafe "hiredis/redis_db.c setCells" c_setCells :: CString -> CInt -> IO ()
foreign import ccall unsafe "hiredis/redis_db.c clearSheet" c_clearSheet :: CString -> IO ()


----------------------------------------------------------------------------------------------------------------------
-- Private DB functions

getCellsByKeys :: [B.ByteString] -> IO [Maybe ASCell]
getCellsByKeys keys = getCellsByMessage msg num
  where
    msg      = B.concat $ [BC.pack "\"", internal, BC.pack "\"\NUL"]
    internal = B.intercalate (BC.pack msgPartDelimiter) keys
    num      = length keys

-- takes a message and number of locations queried
getCellsByMessage :: B.ByteString -> Int -> IO [Maybe ASCell]
getCellsByMessage msg num = do
  ptrCells <- BU.unsafeUseAsCString msg $ \str -> c_getCells str (fromIntegral num)
  cCells   <- peekArray (fromIntegral num) ptrCells
  res      <- mapM cToASCell cCells
  --free ptrCells
  return res

getCellsByKeyPattern :: Connection -> String -> IO [ASCell]
getCellsByKeyPattern conn pattern = runRedis conn $ do
  Right locKeys <- keys . BC.pack $ pattern
  catMaybes <$> (liftIO $ getCellsByKeys locKeys)

setCellsByMessage :: B.ByteString -> Int -> IO ()
setCellsByMessage msg num = BU.unsafeUseAsCString msg $ \lstr -> c_setCells lstr (fromIntegral num)

deleteLocRedis :: ASIndex -> Redis ()
deleteLocRedis loc = del [makeLocationKey loc] >> return ()

getSheetLocsRedis :: ASSheetId -> Redis [B.ByteString]
getSheetLocsRedis sheetid = do
  Right keys <- smembers $ makeSheetSetKey sheetid
  return keys

deleteLocsInSheet :: ASSheetId -> IO ()
deleteLocsInSheet sid = withCString (T.unpack sid) c_clearSheet

cToASCell :: CString -> IO (Maybe ASCell)
cToASCell str = do
  str' <- peekCString str
  let str'' = read ('"':str' ++ ['"']) -- Need to unescape the string (what's passed to Redis was escaped by Bytestring.show)
  return $ case str'' of
    "Nothing" -> Nothing
    otherwise -> Just (read2 str'' :: ASCell)

----------------------------------------------------------------------------------------------------------------------
-- Fat cells

--getListType :: ListKey -> String
--getListType key = last parts
--  where parts = splitBy keyPartDelimiter key
indexIsHead :: ASIndex -> RangeKey -> Bool
indexIsHead idx (RangeKey idx' _) = idx == idx'

rangeKeyToIndices :: RangeKey -> [ASIndex]
rangeKeyToIndices (RangeKey idx dims) = rangeToIndices range
  where
    Index sid (col, row) = idx
    (height, width)      = dims
    range                = Range sid ((col, row), (col+width-1, row+height-1))

getFatCellIntersections :: Connection -> Either [ASIndex] [RangeKey] -> IO [RangeKey]
getFatCellIntersections conn (Left locs) = do
  rangeKeys <- concat <$> mapM (getRangeKeysInSheet conn) (L.nub $ map locSheetId locs)
  return $ filter keyIntersects rangeKeys
  where
    keyIntersects k             = anyLocsContainedInRect locs (rangeRect k)
    anyLocsContainedInRect ls r = any id $ map (indexInRect r) ls
    indexInRect ((a',b'),(a2',b2')) (Index _ (a,b)) = a >= a' && b >= b' &&  a <= a2' && b <= b2'

getFatCellIntersections conn (Right keys) = do
  rangeKeys <- concat <$> mapM  (getRangeKeysInSheet conn) (L.nub $ map (locSheetId . keyIndex) keys)
  printObj "Checking intersections against keys" keys
  return $ L.intersectBy keysIntersect rangeKeys keys
    where 
      rectsIntersect ((y,x),(y2,x2)) ((y',x'),(y2',x2'))
        | y2 < y' = False 
        | y > y2' = False
        | x2 < x' = False 
        | x > x2' = False
        | otherwise = True 
      keysIntersect k1 k2 = rectsIntersect (rangeRect k1) (rangeRect k2)

rangeRect :: RangeKey -> Rect
rangeRect (RangeKey idx dims) = ((col, row), (col + width - 1, row + height - 1))
  where Index _ (col, row) = idx
        (height, width)    = dims

getRangeKeysInSheet :: Connection -> ASSheetId -> IO [RangeKey]
getRangeKeysInSheet conn sid = runRedis conn $ do
  Right keys <- smembers $ makeSheetRangesKey sid
  liftIO $ printObj "GOT RANGEKEYS IN SHEET: " keys
  return $ map (\k -> read2 (BC.unpack k) :: RangeKey) keys

rangeKeyToSheetId :: RangeKey -> ASSheetId
rangeKeyToSheetId = locSheetId . keyIndex

decoupleCell :: ASCell -> ASCell
decoupleCell (Cell l (Coupled _ lang _ _) v ts) = Cell l e' v ts
  where e' = case v of 
               NoValue   -> Expression "" lang
               otherwise -> Expression (showPrimitive lang v) lang

-- | Converts a coupled cell to a normal cell
toUncoupled :: ASCell -> ASCell
toUncoupled c@(Cell _ (Coupled xp lang _ _) _ _) = c { cellExpression = Expression xp lang }

cellToRangeKey :: ASCell -> Maybe RangeKey
cellToRangeKey (Cell _ xp _ _ ) = case xp of 
  Coupled _ _ _ key -> Just key
  _ -> Nothing

isFatCellMember :: ASCell -> Bool
isFatCellMember (Cell _ xp _ _) = case xp of 
  Coupled _ _ _ _ -> True
  _ -> False

isFatCellHead :: ASCell -> Bool 
isFatCellHead cell = case (cellToRangeKey cell) of 
  Just (RangeKey idx _) -> cellLocation cell == idx
  Nothing -> False

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

showB :: (BS.Show a) => a -> B.ByteString
showB a = toStrict2 $ BL.snoc (BS.show $! a) (0::Word8)

----------------------------------------------------------------------------------------------------------------------
-- Reading bytestrings

bStrToASExpression :: Maybe B.ByteString -> Maybe ASExpression
bStrToASExpression (Just b) = Just (read2 (BC.unpack b) :: ASExpression)
bStrToASExpression Nothing = Nothing

bStrToASValue :: Maybe B.ByteString -> Maybe ASValue
bStrToASValue (Just b) = Just (read2 (BC.unpack b) :: ASValue)
bStrToASValue Nothing = Nothing

bStrToTags :: Maybe B.ByteString -> Maybe ASCellProps
bStrToTags (Just b) = Just (read (BC.unpack b) :: ASCellProps)
bStrToTags Nothing = Nothing

maybeASCell :: (ASIndex, Maybe ASExpression, Maybe ASValue, Maybe ASCellProps) -> Maybe ASCell
maybeASCell (l, Just e, Just v, Just tags) = Just $ Cell l e v tags
maybeASCell _ = Nothing

bStrToASIndex :: B.ByteString -> ASIndex
bStrToASIndex b = (read2 (BC.unpack b) :: ASIndex)

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

bStrToRangeDescriptor :: Maybe B.ByteString -> Maybe RangeDescriptor
bStrToRangeDescriptor Nothing = Nothing
bStrToRangeDescriptor (Just str) = Just (read (BC.unpack str) :: RangeDescriptor)

bStrToRangeKeys :: Maybe B.ByteString -> Maybe [RangeKey]
bStrToRangeKeys Nothing = Nothing
bStrToRangeKeys (Just str) = Just (read (BC.unpack str) :: [RangeKey])
