module AS.Eval.Endware where

import Prelude
import AS.Types.Cell
import AS.Types.Commits
import AS.Types.Network
import AS.Types.Eval
import AS.Types.Errors

import AS.Eval.CondFormat
import AS.Util as U
import AS.DB.API as DB
import AS.Daemon as DM


import Control.Monad.Trans.Class (lift)
import qualified Network.WebSockets as WS
import Database.Redis as R
import Control.Concurrent


-- | Here, we apply a stack of endwares.
-- | Endware for producing tags post-eval e.g. streaming or styling
-- | Examples: green(x) in python -> produces styled value with string in output -> string parsed to Color tag
-- | Bloomberg(x) in java -> produces json with stream specs -> converted to Stream tag, kickoff daemon

evalEndware :: MVar ServerState -> [ASCell] -> CommitSource -> [ASCell] -> EvalContext -> EitherTExec [ASCell]
evalEndware state finalCells (sid, uid) origCells ctx = do 
  conn <- lift $ dbConn <$> readMVar state
  mapM_ (\c -> lift $ DM.possiblyCreateDaemon state uid c) origCells
  let cells1 = changeExcelExpressions finalCells
  cells2 <- conditionallyFormatCells conn sid cells1 ctx
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