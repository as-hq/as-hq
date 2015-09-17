module AS.DB.Util where

import Prelude

import AS.Types 
import AS.Util
import AS.Parsing.Common (tryParseListNonIso)
import AS.Parsing.In (int)

import qualified Data.List as L

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B 
import Database.Redis hiding (decode, Message)
import Data.List.Split

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
-- | ByteString and Maybe conversions  

bStrToASExpression :: Maybe B.ByteString -> Maybe ASExpression
bStrToASExpression (Just b) = Just (read (B.unpack b) :: ASExpression)
bStrToASExpression Nothing = Nothing

bStrToASValue :: Maybe B.ByteString -> Maybe ASValue
bStrToASValue (Just b) = Just (read (B.unpack b) :: ASValue)
bStrToASValue Nothing = Nothing     

bStrToTags :: Maybe B.ByteString -> Maybe [ASCellTag]
bStrToTags (Just b) = Just (read (B.unpack b) :: [ASCellTag])
bStrToTags Nothing = Nothing 

maybeASCell :: (ASLocation,Maybe ASExpression,Maybe ASValue, Maybe [ASCellTag]) -> Maybe ASCell
maybeASCell (l, Just e, Just v, Just tags) = Just $ Cell l e v tags
maybeASCell _ = Nothing

maybeASCellFromFields :: [Maybe B.ByteString] -> Maybe ASCell
maybeASCellFromFields ((Just locstr):(Just xpstr):(Just valstr):(Just tagstr):[]) = 
  Just $ Cell l e v ts
  where
    l = read (B.unpack locstr) :: ASLocation
    e = read (B.unpack xpstr) :: ASExpression
    v = read (B.unpack valstr) :: ASValue
    ts = read (B.unpack tagstr) :: [ASCellTag]
maybeASCellFromFields _ = Nothing

bStrToASLocation :: B.ByteString -> ASLocation
bStrToASLocation b = (read (B.unpack b) :: ASLocation)

bStrToRelation :: (B.ByteString,B.ByteString) -> (ASLocation,ASLocation)
bStrToRelation (r,s) = (bStrToASLocation r, bStrToASLocation s)

bStrToASCommit :: Maybe B.ByteString -> Maybe ASCommit 
bStrToASCommit (Just b) = Just (read (B.unpack b) :: ASCommit)
bStrToASCommit Nothing = Nothing

bStrToSheet :: Maybe B.ByteString -> Maybe ASSheet
bStrToSheet (Just b) = Just (read (B.unpack b) :: ASSheet)
bStrToSheet Nothing = Nothing

bStrToWorkbook :: Maybe B.ByteString -> Maybe ASWorkbook
bStrToWorkbook (Just b) = Just (read (B.unpack b) :: ASWorkbook)
bStrToWorkbook Nothing = Nothing

----------------------------------------------------------------------------------------------------------------------
-- | Redis key utilities

tuple3 :: a -> b -> c -> (a,b,c)
tuple3 a b c = (a,b,c)

getLocationKey :: ASLocation -> B.ByteString
getLocationKey (Index sheetid idx) = B.pack $ (show sheetid) ++ ('|':(show idx))
getLocationKey (Range sheetid rng) = B.pack $ (show sheetid) ++ ('|':(show rng))

getSheetKey :: ASSheetId -> B.ByteString -- for storing the actual sheet as key-value
getSheetKey = B.pack . show

getSheetSetKey :: ASSheetId -> B.ByteString -- for storing set of locations in single sheet
getSheetSetKey sid = B.pack $ (show sid) ++ "Locations"

getWorkbookKey :: String -> B.ByteString
getWorkbookKey = B.pack

cellFields :: [B.ByteString]
cellFields = [(B.pack "cellLocation"), (B.pack "cellExpression"), (B.pack "cellValue"), (B.pack "cellTags")]

keyToRow :: B.ByteString -> Int
keyToRow str = row
  where
    (col, row) = read idxStr :: (Int, Int)
    idxStr = B.unpack $ last $ B.split '|' str

incrementLocKey :: (Int, Int) -> B.ByteString -> B.ByteString
incrementLocKey (dx, dy) key = B.pack $ ks ++ '|':kidx
  where
    (sh:idxStr:[]) = B.split '|' key
    ks = B.unpack sh
    (col, row) = read (B.unpack idxStr) :: (Int, Int)
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
-- | Private functions

getCellByKeyRedis :: B.ByteString -> Redis (Maybe ASCell)
getCellByKeyRedis key = do
    Right strs <- hmget key cellFields
    return $ maybeASCellFromFields strs

setCellRedis :: ASCell -> Redis ()
setCellRedis cell = do
    let loc = cellLocation cell
        key = getLocationKey loc
        locstr = (B.pack . show) loc
        xpstr = (B.pack . show) (cellExpression cell)
        valstr = (B.pack . show) (cellValue cell)
        tagstr = (B.pack . show) (cellTags cell)
        hash = zip cellFields [locstr, xpstr, valstr, tagstr]
    _ <- hmset key hash -- set cell hash
    let setKey = getSheetSetKey (locSheetId loc)
    _ <- sadd setKey [key] -- add the location key to the set of locs in a sheet (for sheet deletion etc)
    return ()

deleteLocRedis :: ASLocation -> Redis ()
deleteLocRedis loc = del [getLocationKey loc] >> return ()

getSheetLocsRedis :: ASSheetId -> Redis [B.ByteString]
getSheetLocsRedis sheetid = do
  Right keys <- smembers $ getSheetSetKey sheetid
  return keys

updateChunkDAG :: [([ASLocation],ASLocation)] -> Redis ()
updateChunkDAG rels = do 
  let update = filter (\(a,b) -> (a /= [])) $ map (\(a,b) -> (map (B.pack . show) a, (B.pack . show) b)) rels
  if (update == [])
    then return ()
    else do 
      TxSuccess _ <- multiExec $ do 
          s1 <- mapM (\(a,b) -> sadd b a) update
          s2 <- sadd (B.pack "DAGLocSet") (map snd update)
          return s2
      return ()

chunkM_ :: Connection -> ([a] -> Redis ()) -> (Int) -> [a] -> IO ()
chunkM_ conn f size lst = do 
  let chunks = chunksOf size lst
  runRedis conn $ mapM_ f chunks

getLastRowKey :: [B.ByteString] -> B.ByteString
getLastRowKey keys = maxBy keyToRow keys
