module AS.Eval.Endware where

import Prelude
import AS.Types.Cell
import AS.Types.Commits
import AS.Types.Network
import AS.Types.Eval
import AS.Types.Errors
import AS.Types.Updates

import AS.Eval.CondFormat
import AS.Util
import AS.Logging
import AS.DB.API as DB
import AS.Daemon as DM


import Control.Monad.Trans.Class (lift)
import qualified Network.WebSockets as WS
import Database.Redis as R
import Control.Concurrent
import Control.Lens hiding ((.=))


-- | Here, we apply a stack of endwares for setting props post-eval, from e.g. streaming or conditional formatting
evalEndware :: MVar ServerState -> CommitSource -> EvalContext -> EitherTExec [ASCell]
evalEndware mstate (CommitSource sid uid) ctx = do 
  state <- lift $ readMVar mstate
  let conn = state^.dbConn
      kernelAddress = state^.appSettings.pyKernelAddress
      cells0 = newCellsInContext ctx
      cells1 = changeExcelExpressions cells0
      blankedCells = blankCellsAt (refsToIndices . oldKeys . cellUpdates . updateAfterEval $ ctx)
  mapM_ (lift . DM.possiblyCreateDaemon mstate uid) cells0
  oldRules <- lift $ DB.getCondFormattingRulesInSheet conn sid 
  let updatedRules = applyUpdate (condFormatRulesUpdates $ updateAfterEval ctx) oldRules
  cells2 <- conditionallyFormatCells kernelAddress conn sid (cells1 ++ blankedCells) updatedRules ctx
  return cells2
   
----------------------------------------------------------------------------------------------------------------------------------------------
-- Endwares

tagStyledCells :: [ASCell] -> [ASCell]
tagStyledCells = id

changeExcelExpressions :: [ASCell] -> [ASCell]
changeExcelExpressions = id
-- L.map upperCase
-- 	where
-- 		upperCase :: ASCell -> ASCell
-- 		upperCase (Cell l (Expression e Excel) v t) = Cell l (Expression e' Excel) v t 
-- 			where 
-- 				e' = L.map toUpper e
-- 		upperCase c = c
-- #incomplete should change all function names to upper-cased forms