{-# LANGUAGE OverloadedStrings, TemplateHaskell, DefaultSignatures, 
DeriveGeneric, TypeOperators, FlexibleContexts #-}

module Conv where

-- Basic
import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe hiding (fromJust)
import Control.Applicative
import Control.Concurrent
import Control.Monad
import GHC.Generics

-- Redis
import Database.Redis hiding (decode)
-- ByteString
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.SafeCopy (SafeCopy)
import qualified Data.Text as T

-- Our Types
import Prelude()
import AS.Prelude
import AS.Types.Cell
import AS.Types.Bar
import AS.Types.BarProps (BarProp, ASBarProps) 
import AS.Types.Messages
import AS.Types.EvalHeader
import AS.Types.CellProps
import AS.Types.Errors
import AS.Types.Eval
import AS.Types.CondFormat
import AS.Types.Updates
import AS.Types.Commits
import AS.Types.DB
import AS.Types.Graph
import AS.Types.User
import qualified AS.Serialize as S
import AS.Util

import Types

import Text.Read (readMaybe)
import Debug.Trace

------------------------------------------------------------------------------------------------------
-- Configuration

isFirstMigration :: Bool
isFirstMigration = True

------------------------------------------------------------------------------------------------------
-- Helpers

exhaustiveParser :: [ByteString -> Maybe a] -> ByteString -> Maybe a
exhaustiveParser [] b = Nothing
exhaustiveParser (f:fs) b = case f b of
  Nothing -> exhaustiveParser fs b
  Just x  -> Just x

------------------------------------------------------------------------------------------------------
-- Decoding and encoding DBValues

-- Given a DBKey and a ByteString associated with its value, get the new value 
-- after decoding and encoding. We case separately on EvalHeaders because they have
-- ByteStrings as values, and we don't want parsing to always succeed. 
-- Ignore LastMessageQueries for now (screw up probably bc they're Acknowledges, which don't exist)
newValueBS :: DBKey -> ByteString -> Maybe ByteString
newValueBS (LastMessageKey _) b = Just b
newValueBS (EvalHeaderKey _ _) b = if isFirstMigration
  then Just (S.encode $ HeaderValue b) -- Headers were originally just bytestrings
  else S.encode <$> ((toDBValue b) :: Maybe DBValue)
newValueBS _ b = S.encode <$> ((toDBValue b) :: Maybe DBValue)

toDBKey :: ByteString -> Maybe DBKey
toDBKey = if isFirstMigration 
  then exhaustiveParser 
    [ toSheetRangesKey
    , toSheetKey
    , toEvalHeaderKey
    , toTempCommitKey
    , toPushCommitKey
    , toPopCommitKey
    , toLastMessageKey
    , toRedisRangeKey
    , toIndexKey
    , toAllSheetsKey
    , toBarKey 
    ] 
  else S.maybeDecode

toDBValue :: ToDBValue
toDBValue = if isFirstMigration 
  then exhaustiveParser
    [ toKeyValue
    , toSheetValue
    , toCellValue
    , toCommitValue
    , toCFValue
    , toSheetValue
    , toBarValue
    , toCFValue
    , toIndexValue
    , toRangeDescriptorValue
    , toUserValue
    ]
  else S.maybeDecode


------------------------------------------------------------------------------------------------------
-- Decodeable functions for keys

-- Initially, the decodable instances for the keys need to be read2 methods
-- (the inverse of the current toRedisFormat). After that, S.decodeMaybe on a bytestring 
-- key from the DB can be cast to a RedisQuery immediately, so there's no need for this.

delim = "?"
sep = "~"

type ToDBKey = ByteString -> Maybe DBKey

getRest :: String -> ByteString -> Maybe String 
getRest prefix b = do 
  let str = BC.unpack b 
  stripPrefix (prefix ++ sep) str

toCommitSource :: String -> CommitSource
toCommitSource s = CommitSource (T.pack sid) (T.pack uid)
  where
    [sid, uid] = splitOn delim s

toSheetRangesKey :: ToDBKey
toSheetRangesKey b = (SheetGroupKey . SheetRangesKey . T.pack) <$> getRest "SheetRangesType" b

toSheetKey :: ToDBKey
toSheetKey b = (SheetKey . T.pack) <$> getRest "SheetType" b

toEvalHeaderKey :: ToDBKey
toEvalHeaderKey b = do 
  rest <- getRest "EvalHeaderType" b
  let [sid, lang] = splitOn delim rest
  return $ EvalHeaderKey (T.pack sid) ($read lang :: ASLanguage)

toTempCommitKey :: ToDBKey
toTempCommitKey b = (TempCommitKey . toCommitSource) <$> getRest "TempCommitType" b

toPushCommitKey :: ToDBKey
toPushCommitKey b = (PushCommitKey . toCommitSource) <$> getRest "PushCommitType" b

toPopCommitKey :: ToDBKey
toPopCommitKey b = (PopCommitKey . toCommitSource) <$> getRest "PopCommitType" b

toLastMessageKey :: ToDBKey
toLastMessageKey b = (LastMessageKey . toCommitSource) <$> getRest "LastMessageType" b

toAllSheetsKey :: ToDBKey
toAllSheetsKey b = (\_ -> AllSheetsKey) <$> getRest "AllSheetsType" b

toRedisRangeKey :: ToDBKey
toRedisRangeKey b = do 
  rest <- getRest "RangeType" b
  let [ind, dim] = splitOn delim rest
  return $ RedisRangeKey $ RangeKey ((read2 ind) :: ASIndex) ((read2 dim) :: Dimensions)

toIndexKey :: ToDBKey
toIndexKey b = IndexKey <$> S.maybeDecode b

toBarKey :: ToDBKey
toBarKey b = do
  rest <- getRest "BarType2" b
  let [sidStr, typStr, indStr] = splitOn delim rest
  let sid = T.pack sidStr
  let typ = $read typStr :: BarType0
  let ind = $read indStr :: Int
  let barInd = case typ of
                 ColumnType -> BarIndex sid (BarCol $ Col ind)
                 RowType -> BarIndex sid (BarRow $ Row ind)
  return $ BarKey barInd

------------------------------------------------------------------------------------------------------
-- Decodeable functions for values

type ToDBValue = ByteString -> Maybe DBValue 

-- The DB stores show of ASSheet0, but the ASSheet type no longer has sheetPermissions. 
toSheetValue :: ToDBValue
toSheetValue b = do 
  let str = BC.unpack b
  let (first:_) = splitOn ", sheetPermissions" str
  let newSheetStr = first ++ "}"
  let fixSheetName (Sheet id name) = (Sheet id (T.unpack id))
  -- Make sure that the sheet names and ids are the same
  let mDBValue = (SheetValue . fixSheetName) <$> readMaybe newSheetStr
  case mDBValue of
    Nothing -> mDBValue
    Just v -> trace (show v) mDBValue

toCommitValue :: ToDBValue
toCommitValue b = CommitValue <$> S.maybeDecode b

toCellValue :: ToDBValue 
toCellValue b = CellDBValue <$> S.maybeDecode b

toServerMessageValue :: ToDBValue
toServerMessageValue b = ServerMessageValue <$> S.maybeDecode b

toBarValue :: ToDBValue
toBarValue b = BarValue <$> S.maybeDecode b

toCFValue :: ToDBValue
toCFValue b = CFValue <$> S.maybeDecode b

toIndexValue :: ToDBValue
toIndexValue b = IndexValue <$> S.maybeDecode b

toRangeDescriptorValue :: ToDBValue
toRangeDescriptorValue b = RangeDescriptorValue <$> S.maybeDecode b

toUserValue :: ToDBValue
toUserValue b = UserValue <$> S.maybeDecode b

toKeyValue :: ToDBValue
toKeyValue b = KeyValue <$> toDBKey b

