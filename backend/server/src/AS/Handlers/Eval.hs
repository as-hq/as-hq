module AS.Handlers.Eval where

import AS.Prelude
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

handleEval :: MessageId -> ASUserClient -> ServerState -> [EvalInstruction] -> IO ()
handleEval mid uc state evalInstructions  = do
  let xps  = map evalXp evalInstructions
      inds = map evalLoc evalInstructions
      conn = state^.dbConn
  oldProps <- mapM (getPropsAt conn) inds
  let cells = map (\(xp, ind, props) -> Cell ind xp NoValue props Nothing Nothing) $ zip3 xps inds oldProps
  errOrUpdate <- runDispatchCycle state mid cells DescendantsWithParent (userCommitSource uc) id
  broadcastErrOrUpdate mid state uc errOrUpdate

-- not maintaining right now (Alex 12/28)
-- handleEvalRepl :: ASUserClient -> ASPayload -> IO ()
-- handleEvalRepl uc (PayloadXp xp) = do
--   let sid = userSheetId uc
--   msg' <- runReplDispatch sid xp
--   sendToOriginal uc msg'

handleEvalHeader :: MessageId -> ASUserClient -> ServerState -> EvalHeader -> IO ()
handleEvalHeader mid uc state evalHeader = do
  setEvalHeader (state^.dbConn) evalHeader
  result <- runEitherT $ evaluateHeader mid evalHeader
  broadcastTo state [evalHeader^.evalHeaderSheetId] $ case result of 
        Left e -> failureMessage mid $ generateErrorMessage e
        Right (EvalResult value display) -> 
          let lang = evalHeader^.evalHeaderLang
              valueStr = showValue lang value
              headerResult = HeaderResult valueStr display
          in ClientMessage mid (HandleEvaluatedHeader evalHeader headerResult (uc^.userId)) 

-- The user has said OK to the decoupling
-- We've stored the changed range keys and the last commit, which need to be used to modify DB
handleDecouple :: MessageId -> ASUserClient -> ServerState -> IO ()
handleDecouple mid uc state = do 
  let conn = state^.dbConn
      src = userCommitSource uc
  mCommit <- getTempCommit conn src
  case mCommit of
    Nothing -> return ()
    Just c -> do
      updateDBWithCommit conn src c
      broadcastSheetUpdate mid state $ sheetUpdateFromCommit c

handleSetLanguagesInRange :: MessageId -> ASUserClient -> ServerState -> ASLanguage -> ASRange -> IO ()
handleSetLanguagesInRange mid uc state lang rng = do 
  let inds = finiteRangeToIndices rng
      conn = state^.dbConn
  cells <- catMaybes <$> getCells conn inds -- disregard cells that are empty
  let cellsWithLangsChanged = map (cellExpression.language .~ lang) cells
  errOrUpdate <- runDispatchCycle state mid cellsWithLangsChanged DescendantsWithParent (userCommitSource uc) id
  broadcastErrOrUpdate mid state uc errOrUpdate

-- | The user has pressed the "kill" button for an overlong operation;
-- look up the relevant thread, and kill it if it exists.
handleTimeout :: MessageId -> State -> IO ()
handleTimeout mid state = 
  modifyState_ state $ \curState -> do
    putStrLn $ "Killing message ID: " ++ (T.unpack mid)
    case M.lookup mid (curState^.threads) of 
      Just tid -> putStrLn ("killed thread: " ++ (show tid)) >> killThread tid
      _ -> return ()
    Python.haltMessage mid
    R.haltMessage mid
    return $ curState & threads %~ (M.delete mid)
