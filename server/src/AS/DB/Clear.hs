{-# LANGUAGE OverloadedStrings, DataKinds, GADTs #-}

module AS.DB.Clear where

import Prelude
import AS.Types.Cell
import AS.Types.Bar
import AS.Types.BarProps (BarProp, ASBarProps) 
import AS.Types.Messages
import AS.Types.DB
import AS.Types.CellProps
import AS.Types.Errors
import AS.Types.Eval
import AS.Types.CondFormat
import AS.Types.Updates

import qualified AS.DB.API as DB
import qualified AS.Config.Settings as Settings
import AS.Util as U
import qualified AS.DB.Internal as DI
import qualified AS.Kernels.Python.Eval as KP
import AS.Parsing.Substitutions (getDependencies)
import AS.Window
import AS.Logging

import Data.List (zip4,head,nub,intercalate)
import Data.Maybe 
import Data.List.Split
import qualified Data.Map as M

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Database.Redis hiding (decode)

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either


clear :: Connection -> IO ()
clear conn = runRedis conn $ flushall >> return ()

clearSheet :: Connection -> ASSheetId -> IO ()
clearSheet conn sid = 
  let sheetRangesKey = toRedisFormat $ SheetRangesKey sid
      evalHeaderKeys = map (toRedisFormat . (EvalHeaderKey sid)) Settings.headerLangs
      rangeKeys = map (toRedisFormat . RedisRangeKey) <$> DI.getRangeKeysInSheet conn sid
      pluralKeyTypes = [BarType2, PushCommitType, PopCommitType, TempCommitType, LastMessageType, CFRuleType]
      pluralKeys = (evalHeaderKeys ++) . concat <$> mapM (DI.getKeysInSheetByType conn sid) pluralKeyTypes
  in runRedis conn $ do
    pluralKeys <- liftIO pluralKeys
    rangeKeys <- liftIO rangeKeys
    del $ sheetRangesKey : pluralKeys ++ rangeKeys
    liftIO $ DB.deleteLocsInSheet conn sid
    -- clear sheet namespace in pyclearSheetthon kernel
    liftIO $ KP.clear sid