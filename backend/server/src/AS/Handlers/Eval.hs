module AS.Handlers.Eval where

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

import qualified AS.Kernels.Python.Client as Python
import qualified AS.Kernels.R.Client as R

import AS.DB.API
import AS.DB.Transaction
import AS.Reply

import AS.Parsing.Show (showValue)

import qualified Data.Map as M
import qualified Data.Text as T

import Control.Concurrent
import Control.Lens hiding ((.=))
import Control.Monad.Trans.Either

import Data.Maybe (catMaybes)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval handler

handleEval :: MessageContext -> [EvalInstruction] -> IO ()
handleEval msgctx evalInstructions  = do
  let xps  = map evalXp evalInstructions
      inds = map evalLoc evalInstructions
      conn = msgctx^.dbConnection
  oldProps <- mapM (getPropsAt conn) inds
  let cells = map (\(xp, ind, props) -> Cell ind xp NoValue props Nothing Nothing) $ zip3 xps inds oldProps
  errOrUpdate <- runDispatchCycle msgctx cells DescendantsWithParent id
  broadcastErrOrUpdate msgctx errOrUpdate

handleEvalHeader :: MessageContext -> EvalHeader -> IO ()
handleEvalHeader msgctx evalHeader = do
  let mid = msgctx^.messageId
      uid = msgctx^.userClient.userId
  setEvalHeader (msgctx^.dbConnection) evalHeader
  result <- runEitherT $ evaluateHeader mid evalHeader
  broadcastActionTo msgctx [evalHeader^.evalHeaderSheetId] $ case result of 
        Left e -> ShowFailureMessage $ generateErrorMessage e
        Right (EvalResult value display) -> 
          let lang = evalHeader^.evalHeaderLang
              valueStr = showValue lang value
              headerResult = HeaderResult valueStr display
          in HandleEvaluatedHeader evalHeader headerResult uid 

-- The user has said OK to the decoupling
-- We've stored the changed range keys and the last commit, which need to be used to modify DB
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
  let cellsWithLangsChanged = map (cellExpression.language .~ lang) cells
  errOrUpdate <- runDispatchCycle msgctx cellsWithLangsChanged DescendantsWithParent id
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
