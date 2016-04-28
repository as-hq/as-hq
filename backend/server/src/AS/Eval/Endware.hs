module AS.Eval.Endware where

import Control.Monad.Trans.Class (lift)
import qualified Network.WebSockets as WS
import Database.Redis as R
import Control.Concurrent
import Control.Lens hiding ((.=))

import AS.Prelude
import AS.Types.Cell
import AS.Types.Commits
import AS.Types.Network
import AS.Types.Eval
import AS.Types.Errors
import AS.Types.Updates
import AS.Types.Messages (MessageId)

import AS.Eval.CondFormat
import AS.Util
import AS.Logging
import AS.DB.API as DB
import AS.DB.Eval
import AS.Daemon as DM

-- | Here, we apply a stack of endwares for setting props post-eval, from e.g. streaming or conditional formatting
evalEndware ::  MessageContext ->
                EvalContext -> 
                EvalChainFunc -> 
                EitherTExec [ASCell]
evalEndware msgctx evalctx f = do 
  let cells0 = newCellsInContext evalctx
      cells1 = cells0 ++ blankCellsAt (refsToIndices $ evalctx^.updateAfterEval.cellUpdates.oldKeys)
      -- ^ represents all the cells that might have changed from the eval. we don't explicitly record deleted blank cells.
  oldRules <- lift $ DB.getCondFormattingRulesInSheet (msgctx^.dbConnection) (messageSheetId msgctx) 
  let updatedRules = applyUpdate (evalctx^.updateAfterEval.condFormatRuleUpdate) oldRules
  cells2 <- conditionallyFormatCells msgctx evalctx cells1 updatedRules f
  return cells2 -- we added blank cells at the deleted locations -- we don't want the actual Update to remember these. 
