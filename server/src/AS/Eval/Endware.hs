module AS.Eval.Endware where

import Prelude
import AS.Types.Cell
import AS.Types.Commits
import AS.Types.Network
import AS.Types.Eval
import AS.Types.Errors
import AS.Types.Updates

import AS.Eval.CondFormat
import AS.Util as U
import AS.DB.API as DB
import AS.Daemon as DM


import Control.Monad.Trans.Class (lift)
import qualified Network.WebSockets as WS
import Database.Redis as R
import Control.Concurrent


-- | Here, we apply a stack of endwares for setting props post-eval, from e.g. streaming or conditional formatting
evalEndware :: MVar ServerState -> CommitSource -> EvalContext -> EitherTExec [ASCell]
evalEndware state (CommitSource sid uid) ctx = do 
  conn <- lift $ dbConn <$> readMVar state
  let cells0 = newCellsInContext ctx
      cells1 = changeExcelExpressions cells0
  mapM_ (lift . DM.possiblyCreateDaemon state uid) cells0
  rules <- lift $ DB.getCondFormattingRulesInSheet conn sid 
  cells2 <- conditionallyFormatCells conn sid cells1 rules ctx
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