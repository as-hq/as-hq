
{-# LANGUAGE DeriveGeneric, StandaloneDeriving, OverloadedStrings, BangPatterns #-}

module Lib where

import Prelude

import AS.Types.Network
import AS.Types.DB
import AS.Types.Cell
import AS.Types.CellProps
import AS.Types.Eval
import AS.Types.Messages
import AS.Types.DB
import AS.Types.Updates
import AS.Types.Bar
import AS.Types.BarProps
import AS.Types.CondFormat
import AS.Types.Selection

import AS.Dispatch.Core 
import qualified AS.DB.API as DB
import qualified AS.DB.Graph as G
import qualified AS.DB.Internal as DI
import AS.Window
import AS.Util
import qualified AS.Kernels.Python.Eval as KP

import qualified Database.Redis as R
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import qualified Data.Serialize as S
import qualified Data.Map as M
import qualified Data.HashMap as H
import qualified Data.HashTable.IO as HI
import Data.Hashable
import Control.Lens
import Control.Monad.Trans.Either
import Control.Applicative
import Control.Concurrent (newMVar, MVar)
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import Debug.Trace
import GHC.Generics

import Criterion.Main

deriving instance Generic ServerState
deriving instance Generic ASUserClient
deriving instance Generic ASDaemonClient

instance NFData ServerMessage where rnf = genericRnf
instance NFData ServerAction where rnf = genericRnf
instance NFData ASCell where rnf = genericRnf
instance NFData ASCellProps where rnf = genericRnf
instance NFData ASExpression where rnf = genericRnf
instance NFData ExpandingType where rnf = genericRnf
instance NFData ASLanguage where rnf = genericRnf
instance NFData CellPropType where rnf = genericRnf
instance NFData ASWindow where rnf = genericRnf
instance NFData WorkbookSheet where rnf = genericRnf
instance NFData ASWorkbook where rnf = genericRnf
instance NFData CellProp where rnf = genericRnf
instance NFData RangeKey where rnf = genericRnf
instance NFData ASSheet where rnf = genericRnf
instance NFData ASPermissions where rnf = genericRnf
instance NFData ASCommit where rnf = genericRnf
instance NFData VAlignType where rnf = genericRnf
instance NFData Dimensions where rnf = genericRnf
instance NFData Stream where rnf = genericRnf
instance NFData ASUserEntity where rnf = genericRnf
instance NFData RangeDescriptor where rnf = genericRnf
instance NFData ASUserGroup where rnf = genericRnf
instance NFData StreamSource where rnf = genericRnf
instance NFData ASTime where rnf = genericRnf
instance NFData HAlignType where rnf = genericRnf
instance NFData FormatType where rnf = genericRnf
instance NFData JSONField where rnf = genericRnf
instance NFData JSONValue where rnf = genericRnf
instance NFData MutateType where rnf = genericRnf
instance NFData ASUserClient where rnf = genericRnf
instance NFData ASDaemonClient where rnf = genericRnf
instance NFData ServerState where rnf = genericRnf
instance NFData ASExecError where rnf = genericRnf
instance NFData CommitSource where rnf = genericRnf
instance NFData Bloomberg where rnf = genericRnf
instance (NFData a) => NFData (Diff a) where rnf = genericRnf
instance NFData Bar where rnf = genericRnf
instance NFData BarIndex where rnf = genericRnf
instance NFData BarType where rnf = genericRnf
instance NFData ASBarProps where rnf = genericRnf
instance NFData BarProp where rnf = genericRnf
instance NFData SheetUpdate where rnf = genericRnf
instance NFData BarPropType where rnf = genericRnf
instance (NFData a, NFData b) => NFData (Update a b) where rnf = genericRnf
instance NFData CondFormatRule where rnf = genericRnf
instance NFData CondFormatCondition where rnf = genericRnf
instance NFData NotEqualsCondition where rnf = genericRnf
instance NFData LessThanCondition where rnf = genericRnf
instance NFData Selection where rnf = genericRnf
instance NFData LeqCondition where rnf = genericRnf
instance NFData EvalInstruction where rnf = genericRnf
instance NFData IsNotEmptyCondition where rnf = genericRnf
instance NFData IsNotBetweenCondition where rnf = genericRnf
instance NFData IsEmptyCondition where rnf = genericRnf
instance NFData IsBetweenCondition where rnf = genericRnf
instance NFData GreaterThanCondition where rnf = genericRnf
instance NFData GeqCondition where rnf = genericRnf
instance NFData EqualsCondition where rnf = genericRnf
instance NFData CustomCondition where rnf = genericRnf

-- don't really care about deep evaluating these types
instance NFData WS.Connection where rnf conn = seq conn ()
instance NFData R.Connection where rnf conn = seq conn ()
instance (NFData a) => NFData (MVar a) where rnf x = seq x ()

--instance (NFData k, NFData v) => NFData (HI.IOHashTable HI.HashTable k v) where
--  rnf h = seq h ()

data ASEnv = ASEnv { envConn :: R.Connection, envState :: MVar ServerState, envSource :: CommitSource } deriving (Generic)
instance NFData ASEnv where rnf = genericRnf

yellow :: String -> String
yellow s = "\x1b[33m" ++ s ++ "\x1b[0m"
green :: String -> String
green s = "\x1b[32m" ++ s ++ "\x1b[0m"
emptyBench       = bgroup "" []
it               = bench . green
has a b          = env (setupEnvWith a) b 
describe         = bgroup . green
xdescribe desc _ = trace (yellow $ "skipped group: " ++ desc) emptyBench
xit desc _       = trace (yellow $ "skipped: " ++ desc) emptyBench
run :: NFData b => (a -> b) -> a -> Benchmarkable
run = nf
runIO :: NFData a => IO a -> Benchmarkable
runIO = nfIO

setupEnv :: IO ASEnv
setupEnv = do
  conn <- R.connect DI.cInfo
  state <- newMVar $ State [] [] conn (0 :: Port)
  let src = CommitSource { srcSheetId = "sheetid", srcUserId = "userid" }
  return $ ASEnv conn state src

setupEnvWith :: (NFData a) => a -> IO (ASEnv, a)
setupEnvWith x = (,) <$> setupEnv <*> return x

testCells = map (\i -> testCell & cellLocation .~ Index "BENCH_ID" (1,i))
testMap = toMap . testCells
