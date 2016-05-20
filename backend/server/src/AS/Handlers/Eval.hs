module AS.Handlers.Eval where

import Control.Concurrent
import Control.Lens hiding ((.=))
import Control.Monad.Trans.Either
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import qualified Data.Text as T

import AS.Prelude
import AS.Logging
import AS.Types.Cell
import AS.Types.Messages
import AS.Types.User hiding (userId)
import AS.Types.Commits
import AS.Types.Eval
import AS.Types.EvalHeader
import AS.Types.Network

import AS.Dispatch.Core
import AS.Eval.Core (evaluateHeader)
import AS.Parsing.Show (showValue)
import AS.DB.API
import AS.DB.Transaction
import AS.Reply
import qualified AS.Kernels.Python.Client as Python
import qualified AS.Kernels.R.Client as R

import qualified Data.Map as M
import qualified Data.Text as T

-- Eval handler

handleEval :: MessageContext -> [EvalInstruction] -> IO ()
handleEval msgctx evalInstructions  = do
  let xps  = map evalXp evalInstructions
      inds = map evalLoc evalInstructions
      conn = msgctx^.dbConnection
  oldProps <- mapM (getPropsAt conn) inds
  let defCell (xp, ind, props) = Cell ind xp NoValue props Nothing Nothing
  let cells = map defCell $ zip3 xps inds oldProps
  errOrUpdate <- runDispatchCycle msgctx cells DescendantsWithParent id
  broadcastErrOrUpdate msgctx errOrUpdate

handleEvalHeader :: MessageContext -> EvalHeader -> IO ()
handleEvalHeader msgctx evalHeader = do
  let mid = msgctx^.messageId
      uid = msgctx^.userClient.userId
      conn = msgctx^.dbConnection
  setEvalHeader conn evalHeader
  result <- runEitherT $ evaluateHeader mid evalHeader
  sids <- getWorkbookSheetIds conn $ evalHeader^.evalHeaderWorkbookId 
  broadcastActionTo msgctx sids $ case result of 
        Left e -> ShowFailureMessage $ generateErrorMessage e
        Right (EvalResult value display) -> 
          let lang = evalHeader^.evalHeaderLang
              valueStr = showValue lang value
              headerResult = HeaderResult valueStr display
          in HandleEvaluatedHeader evalHeader headerResult uid 

--------------------------------------------------------------------------------
-- Re-evaluation

handleReEval :: MessageContext -> SheetID -> IO ()
handleReEval msgctx sid = do 
  let conn = msgctx^.dbConnection
  cells <- getCellsInSheet conn sid
  errOrUpdate <- runDispatchCycle msgctx cells DescendantsWithParent id
  broadcastErrOrUpdate msgctx errOrUpdate

--------------------------------------------------------------------------------
-- Misc

-- The user has said OK to the decoupling
-- We've stored the changed range keys and the last commit, which need to be 
-- used to modify the DB
handleDecouple :: MessageContext -> IO ()
handleDecouple msgctx = do 
  let conn = msgctx^.dbConnection
      src = messageCommitSource msgctx
  mCommit <- getTempCommit conn src
  case mCommit of
    Nothing -> return ()
    Just c -> do
      updateDBWithCommit conn src c
      broadcastSheetUpdate msgctx $ sheetUpdateFromCommit c

handleSetLanguagesInRange :: MessageContext -> ASLanguage -> ASRange -> IO ()
handleSetLanguagesInRange msgctx lang rng = do 
  let idxs = finiteRangeToIndices rng
      conn = msgctx^.dbConnection
  cells <- catMaybes <$> getCells conn idxs -- disregard cells that are empty
  let newLangCells = map (cellExpression.language .~ lang) cells
  errOrUpdate <- runDispatchCycle msgctx newLangCells DescendantsWithParent id
  broadcastErrOrUpdate msgctx errOrUpdate

-- | The user has pressed the "kill" button for an overlong operation;
-- look up the relevant thread, and kill it if it exists.
handleTimeout :: State -> MessageId -> IO ()
handleTimeout state timeoutMid = 
  modifyState_ state $ \st -> do
    putsObj "Killing message ID: " timeoutMid
    case M.lookup timeoutMid (st^.threads) of 
      Just tid -> putsObj "killed thread: " tid >> killThread tid
      _ -> return ()
    Python.haltMessage timeoutMid
    R.haltMessage timeoutMid 
    return $ st & threads %~ (M.delete timeoutMid)

--------------------------------------------------------------------------------
