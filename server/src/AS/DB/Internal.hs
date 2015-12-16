{-# LANGUAGE OverloadedStrings, GADTs, DataKinds #-}

module AS.DB.Internal where

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
-- Private DB functions

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

getKeysByType :: Connection -> RedisKeyType -> IO [B.ByteString]
getKeysByType conn = (getKeysByPattern conn) . keyPattern

getKeysInSheetByType :: Connection -> RedisKeyType -> ASSheetId -> IO [B.ByteString]
getKeysInSheetByType conn kt sid = getKeysByPattern conn $ keyPatternBySheet kt sid

getKeysByPattern :: Connection -> String -> IO [B.ByteString]
getKeysByPattern conn pattern = runRedis conn $ fromRight <$> keys (BC.pack pattern)

----------------------------------------------------------------------------------------------------------------------
-- Fat cells

getRangeKeysInSheet :: Connection -> ASSheetId -> IO [RangeKey]
getRangeKeysInSheet conn sid = runRedis conn $ do
  Right ks <- smembers . toRedisFormat $ SheetRangesKey sid
  liftIO $ printObj "GOT RANGEKEYS IN SHEET: " ks
  return $ map (unpackKey . fromRedis) ks
    where
      fromRedis k = read2 (BC.unpack k) :: RedisKey RangeType
      unpackKey :: RedisKey RangeType -> RangeKey
      unpackKey (RedisRangeKey k) = k

toDecoupled :: ASCell -> ASCell
toDecoupled (Cell l (Coupled _ lang _ _) v ts) = Cell l e' v ts
  where e' = case v of 
               NoValue   -> Expression "" lang
               otherwise -> Expression (showPrimitive lang v) lang
toDecoupled c = c

-- | Converts a coupled cell to a normal cell
toUncoupled :: ASCell -> ASCell
toUncoupled c@(Cell _ (Coupled xp lang _ _) _ _) = c { cellExpression = Expression xp lang }

----------------------------------------------------------------------------------------------------------------------
-- DB conversions

bStrToSheet :: Maybe B.ByteString -> Maybe ASSheet
bStrToSheet (Just b) = Just (read (BC.unpack b) :: ASSheet)
bStrToSheet Nothing = Nothing

bStrToWorkbook :: Maybe B.ByteString -> Maybe ASWorkbook
bStrToWorkbook (Just b) = Just (read (BC.unpack b) :: ASWorkbook)
bStrToWorkbook Nothing = Nothing