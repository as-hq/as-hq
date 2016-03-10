{-# LANGUAGE OverloadedStrings, DataKinds, StandaloneDeriving, DeriveGeneric #-}
module Main where
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
deriving instance Generic ServerState
deriving instance Generic ASUserClient
deriving instance Generic EvalContext
instance NFData ASExecError
instance NFData EvalContext
instance NFData SheetUpdate

testSS :: IO ServerState
testSS = alphaMain $ do
  conn <- DI.connectRedis
  return $ emptyServerState conn 

emptyCtx :: EvalContext
emptyCtx = emptyContext
eval :: [ASCell] -> EvalContext -> IO (Either ASExecError SheetUpdate)
eval cells ctx = do
  ss <- liftIO testSS
  runDispatchCycle ss cells DescendantsWithParent (CommitSource "BENCH_ID" "BENCH_ID") id

mockMessageId :: T.Text 
mockMessageId = T.pack ""

testCells :: [Int] -> [ASCell]
testCells = testCellsWithExpression (const $ Expression "=1+1" Excel) 1

testCellsWithExpression :: (Int -> ASExpression) -> Int -> [Int] -> [ASCell]
testCellsWithExpression f col = map (\i -> U.testCell & cellLocation .~ testIndex col i & cellExpression .~ (f i))

testIndex :: Int -> Int -> ASIndex
testIndex x y = Index "BENCH_ID" (Coord x y)

main :: IO ()
main = alphaMain $ do 
  -- let inds = map (\i -> Index "BENCH_ID" (Coord 1 i)) [1..5000]
  -- settings <- CS.getRuntimeSettings
  -- conn <- DI.connectRedis settings
  state <- testSS
  -- y <- eval [(U.testCell & cellExpression .~ Expression "range(5000)" Python)] emptyCtx
  
  -- let conn = state^.dbConn
  --     graphAddress = state^.appSettings.graphDbAddress
  -- commit <- DT.undo graphAddress conn (CommitSource "BENCH_ID" "BENCH_ID")
  let cells1 = testCellsWithExpression (\i -> Expression "=1+1" Excel) 1 [1]
      cells2 = testCellsWithExpression (\i -> Expression ("=A" ++ (show (i-1)) ++ "+1") Excel) 1 [2..8000]
  x <- eval (cells1++cells2) emptyCtx
  evaluate $ rnf x