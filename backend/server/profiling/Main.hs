{-# LANGUAGE OverloadedStrings, DataKinds, StandaloneDeriving, DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import AS.Prelude
import AS.Dispatch.Core
import AS.Config.Settings as CS
import qualified Data.Map as M
import GHC.Generics
import AS.Util as U
import Control.Lens
import qualified Network.WebSockets as WS
import Control.Concurrent

import AS.DB.API

import AS.Handlers.Delete

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
import AS.Types.User
import AS.Types.Window
import AS.Types.Formats
import AS.Types.CondFormat
import AS.Types.EvalHeader
import AS.Types.Commits

import qualified AS.Serialize as S

import Criterion.Main
import Criterion.Main.Options
import Criterion.Types
import qualified AS.DB.Transaction as DT

import AS.Handlers.Misc

import qualified Data.Text as T

import qualified Database.Redis as R
import qualified AS.DB.Internal as DI
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (runEitherT)
import Control.DeepSeq.Generics (genericRnf)
import Control.DeepSeq
import Control.Exception


import AS.Parsing.Substitutions
import AS.Kernels.Excel.Compiler
import AS.Types.Excel hiding (dbConn)
import qualified AS.Kernels.Internal as KI

import qualified AS.DB.Clear as DBC
import qualified AS.DB.Graph as DBG
import qualified AS.DB.API as DB
import qualified AS.DB.Users as DU

import Network.Socket (withSocketsDo)


--------------------------------------------------------------------------------
-- Instances

deriving instance Generic ServerState
deriving instance Generic ASUserClient
deriving instance Generic EvalContext
deriving instance Generic ContextualFormula
deriving instance Generic Formula
deriving instance Generic BasicFormula
deriving instance Generic ExRef
deriving instance Generic ExTemplateExpr
deriving instance Generic ExRange
deriving instance Generic RefType
deriving instance Generic EValue
deriving instance Generic (Formatted a)
deriving instance Generic ENumeric 

instance NFData ASExecError
instance NFData EvalContext
instance NFData SheetUpdate
instance NFData ContextualFormula
instance NFData Formula
instance NFData BasicFormula
instance NFData ExRef
instance NFData ExTemplateExpr
instance NFData ExRange
instance (NFData a) => NFData (ExItem a)
instance NFData RefType
instance NFData EValue
instance (NFData a) => NFData (Formatted a)
instance NFData ENumeric

--------------------------------------------------------------------------------
-- Environment

testSS :: IO ServerState
testSS = alphaMain $ do
  conn <- DI.connectRedis
  return $ emptyServerState conn 

-- | Given the test and a WS server connection, create an initial messageContext
-- with which to run the test, and then run the test. 
clientApp :: (MessageContext -> IO ()) -> WS.Connection -> IO ()
clientApp action wsConn = do 
  ss <- testSS 
  state <- newMVar ss 
  let conn = ss^.dbConn
  clean conn
  uc <- DU.createUserClient conn wsConn "bench_user_id" 
  let msgctx = MessageContext { _messageState = State state
                              , _messageId = "bench_message_id"
                              , _userClient = uc
                              , _dbConnection = conn
                              }
  action msgctx

-- | Each test is a function taking messageContext -> IO (). We augment this
-- by creating a WS connection and running a clientApp.
genTest :: (MessageContext -> IO ()) -> IO ()
genTest action = alphaMain $ do 
  host <- getSetting serverHost
  port <- getSetting serverPort
  withSocketsDo $ WS.runClient host port "/" $ clientApp action 

--------------------------------------------------------------------------------
-- Generating objects

testCells :: [Int] -> [ASCell]
testCells = testCellsWithExpression (const $ Expression "=1+1" Excel) 1

testCellsWithExpression :: (Int -> ASExpression) -> Int -> [Int] -> [ASCell]
testCellsWithExpression f col = map $
  \i -> U.testCell & cellLocation .~ testIndex col i & cellExpression .~ (f i)

testIndex :: Int -> Int -> ASIndex
testIndex x y = Index "BENCH_ID" (Col x, Row y)

--------------------------------------------------------------------------------
-- Clean up

clean :: R.Connection -> IO ()
clean conn = DBC.clear conn >> DBG.clearAllDAGs

--------------------------------------------------------------------------------
-- Benchmarks

dispatchN :: Int -> MessageContext -> IO ()
dispatchN n msgctx = do
  let f = \i -> Expression ("=2+2") Python
  let cells = testCellsWithExpression f 1 [1..n]
  runDispatchCycle msgctx cells DescendantsWithParent id
  return ()

rangeN :: Int -> MessageContext -> IO ()
rangeN n msgctx = do 
  let f = \i -> Expression ("=range(" ++ show n ++ ")") Python
  let cells = testCellsWithExpression f 1 [1]
  runDispatchCycle msgctx cells DescendantsWithParent id
  return ()


main :: IO ()
main = genTest $ dispatchN 10000

--defaultMain [ 
--  bgroup "dispatch"
--    [ bench "dispatch 10000 cells" $ whnfIO $ genTest $ dispatchN 10000
    
--    ]
--  --bgroup "ranges"
--  --  [ bench "range(10000)" $ whnfIO $ genTest $ rangeN 10000

--  --  ]
--  ]

