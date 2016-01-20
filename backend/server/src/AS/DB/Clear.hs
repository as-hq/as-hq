{-# LANGUAGE OverloadedStrings, DataKinds, GADTs #-}

module AS.DB.Clear where

import Prelude()
import AS.Prelude
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
import AS.Types.Network

import qualified AS.DB.API as DB
import qualified AS.Config.Settings as Settings
import AS.Util as U
import qualified AS.DB.Internal as DI
import qualified AS.Kernels.Python as KP
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
import Control.Lens hiding ((.=))
import Database.Redis hiding (decode)

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either


clear :: Connection -> IO ()
clear conn = runRedis conn $ flushall >> return ()

-- #needsrefactor #incomplete could be condensed better; also, LastMessageType doesn't actually get deleted
-- (at least not consistently.)
clearSheet :: AppSettings -> Connection -> ASSheetId -> IO ()
clearSheet settings conn sid = 
  let sheetRangesKey = toRedisFormat $ SheetRangesKey sid
      evalHeaderKeys = map (toRedisFormat . (EvalHeaderKey sid)) Settings.headerLangs
      rangeKeys = map (toRedisFormat . RedisRangeKey) <$> DB.getRangeKeysInSheet conn sid
      pluralKeyTypes = [BarType2, PushCommitType, PopCommitType, TempCommitType, LastMessageType, CFRuleType]
      pluralKeys = (evalHeaderKeys ++) . concat <$> mapM (DI.getKeysInSheetByType conn sid) pluralKeyTypes
  in runRedis conn $ do
    pluralKeys <- liftIO pluralKeys
    rangeKeys <- liftIO rangeKeys
    del $ sheetRangesKey : pluralKeys ++ rangeKeys
    liftIO $ DB.deleteLocsInSheet conn sid

    -- clear python kernel for sheet
    liftIO $ KP.clear (settings^.pyKernelAddress) sid