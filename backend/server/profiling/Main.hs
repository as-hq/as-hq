{-# LANGUAGE OverloadedStrings, DataKinds, StandaloneDeriving, DeriveGeneric, TemplateHaskell #-}
module Main where
import Prelude()
import AS.Prelude
import AS.Dispatch.Core
import AS.Config.Settings as CS
import qualified Data.Map as M
import GHC.Generics
import AS.Util as U
import Control.Lens

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
import Control.Concurrent.MVar

import AS.Parsing.Substitutions
import AS.Kernels.Excel.Compiler
import AS.Types.Excel hiding (dbConn)

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


testSS :: IO ServerState
testSS = alphaMain $ do
  conn <- DI.connectRedis
  return $ emptyServerState conn 

emptyCtx :: EvalContext
emptyCtx = emptyContext
eval :: MessageContext -> [ASCell] -> EvalContext -> IO (Either ASExecError SheetUpdate)
eval msgctx cells evalctx = do
  runDispatchCycle msgctx cells DescendantsWithParent id

mockMessageId :: T.Text 
mockMessageId = T.pack ""

testCells :: [Int] -> [ASCell]
testCells = testCellsWithExpression (const $ Expression "=1+1" Excel) 1

testCellsWithExpression :: (Int -> ASExpression) -> Int -> [Int] -> [ASCell]
testCellsWithExpression f col = map (\i -> U.testCell & cellLocation .~ testIndex col i & cellExpression .~ (f i))

testIndex :: Int -> Int -> ASIndex
testIndex x y = Index "BENCH_ID" (Col x, Row y)

main :: IO ()
main = alphaMain $ do 
  -- let inds = map (\i -> Index "BENCH_ID" (Coord 1 i)) [1..5000]
  -- settings <- CS.getRuntimeSettings
  -- conn <- DI.connectRedis settings
  state <- testSS
  mstate <- newMVar state
  let msgctx = MessageContext { _messageState = State mstate
                              , _messageId = "BENCH_ID"
                              , _userClient = $undefined -- can't create a userclient here. when necessary, fix this.
                              , _dbConnection = state^.dbConn
                              }
  -- let conn = state^.dbConn
  --     graphAddress = state^.appSettings.graphDbAddress
  -- commit <- DT.undo graphAddress conn (CommitSource "BENCH_ID" "BENCH_ID")
  --let cells1 = testCellsWithExpression (\i -> Expression "=1+1" Excel) 1 [1]
  --    cells2 = testCellsWithExpression (\i -> Expression ("=A" ++ (show (i-1)) ++ "+1") Excel) 1 [2..8000]
  --x <- eval (cells1++cells2) emptyCtx
  let cells2 = testCellsWithExpression (\i -> Expression "=2" Python) 1 [1]
  let cells1 = testCellsWithExpression (\i -> Expression "1111111 + $A$1 + 1111111" Python) 1 [2..10000]
  x <- eval msgctx (cells2 ++ cells1) emptyCtx
  -- x <- eval [(U.testCell & cellExpression .~ Expression "range(10000)" Python)] emptyCtx
  evaluate $ rnf x
  ---- let results = map (getExcelReferences) [Expression "=$A1+A1 + 1111 +$A1" Excel | x <- [1..10000]]
  --let results = map parseFormula ["=1111111 + 1111111" | x <- [1..1000000]]
  --evaluate $ rnf results
  --print (take 10 results)