{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

module Main where

import Prelude

import AS.Types.Network
import AS.Types.DB
import AS.Types.Cell
import AS.Types.CellProps
import AS.Types.Eval
import AS.Types.Messages
import AS.Types.DB
import AS.Types.RowColProps

import AS.Dispatch.Core 
import qualified AS.DB.Internal as DI
import AS.Window
import AS.Util
import qualified AS.Kernels.Python.Eval as KP

import qualified Database.Redis as R
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import Control.Monad.Trans.Either
import Control.Concurrent (newMVar, MVar)
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics

import Criterion.Main

deriving instance Generic ServerState
deriving instance Generic ASUserClient
deriving instance Generic ASDaemonClient

instance NFData ASServerMessage where rnf = genericRnf
instance NFData ASPayload where rnf = genericRnf
instance NFData ASResult where rnf = genericRnf
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
instance NFData ASAction where rnf = genericRnf
instance NFData ASCommit where rnf = genericRnf
instance NFData VAlignType where rnf = genericRnf
instance NFData Dimensions where rnf = genericRnf
instance NFData RowColProp where rnf = genericRnf
instance NFData Stream where rnf = genericRnf
instance NFData ASUserEntity where rnf = genericRnf
instance NFData CellDiff where rnf = genericRnf
instance NFData RangeDescriptor where rnf = genericRnf
instance NFData ASUserGroup where rnf = genericRnf
instance NFData StreamSource where rnf = genericRnf
instance NFData ASTime where rnf = genericRnf
instance NFData HAlignType where rnf = genericRnf
instance NFData RowColType where rnf = genericRnf
instance NFData Bloomberg where rnf = genericRnf
instance NFData RowCol where rnf = genericRnf
instance NFData FormatType where rnf = genericRnf
instance NFData JSONField where rnf = genericRnf
instance NFData DescriptorDiff where rnf = genericRnf
instance NFData JSONValue where rnf = genericRnf
instance NFData QueryList where rnf = genericRnf
instance NFData ASRowColProps where rnf = genericRnf
instance NFData MutateType where rnf = genericRnf
instance NFData RowColPropType where rnf = genericRnf
instance NFData Direction where rnf = genericRnf
instance NFData CondFormatRule where rnf = genericRnf
instance NFData ASReplValue where rnf = genericRnf
instance NFData ASInitDaemonConnection where rnf = genericRnf
instance NFData ASInitConnection where rnf = genericRnf
instance NFData ASUserClient where rnf = genericRnf
instance NFData ASDaemonClient where rnf = genericRnf
instance NFData ServerState where rnf = genericRnf

instance NFData WS.Connection where rnf conn = seq conn ()
instance NFData R.Connection where rnf conn = seq conn ()
instance NFData (MVar a) where rnf x = seq x ()

type ASEnv = (R.Connection, MVar ServerState, CommitSource)

setupEnv :: IO ASEnv
setupEnv = do
  conn <- R.connect DI.cInfo
  state <- newMVar $ State [] [] conn (0 :: Port)
  let src = (T.pack "sheetid", T.pack "userid")
  return (conn, state, src)

setupEnvWithCells :: Int -> IO (ASEnv, [ASCell])
setupEnvWithCells n = do
  let cells = take n $ repeat (testCell id)
  e <- setupEnv
  return (e, cells)

main :: IO ()
main = do
  defaultMain [
      env (setupEnvWithCells 1000) $ \ ~((conn, state, src), cells) ->
        bench "dispatch 1000 cells" $ nfIO (runDispatchCycle state cells DescendantsWithParent src)
    ]