module AS.Eval.Endware where

import Prelude()
import AS.Prelude

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
  let cells0 = newCellsInContext ctx
      cells1 = cells0 ++ blankCellsAt (refsToIndices . oldKeys . cellUpdates . updateAfterEval $ ctx)
      -- represents all the cells that might have changed from the eval. we don't explicitly record deleted blank cells.
  mapM_ (lift . DM.possiblyCreateDaemon mstate uid) cells0
  oldRules <- lift $ DB.getCondFormattingRulesInSheet (state^.dbConn) sid 
  let updatedRules = applyUpdate (condFormatRulesUpdates $ updateAfterEval ctx) oldRules
  cells2 <- conditionallyFormatCells state sid cells1 updatedRules ctx
  return cells2 -- we added blank cells at the deleted locations -- we don't want the actual Update to remember these. 
   