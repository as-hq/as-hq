module AS.Handlers.Eval where

import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User
import AS.Types.Commits
import AS.Types.Eval
import AS.Types.EvalHeader
import AS.Types.Network

import AS.Dispatch.Core
--import AS.Dispatch.Repl
import AS.Eval.Core (evaluateHeader)

import AS.DB.API
import AS.DB.Eval
import AS.DB.Expanding
import AS.DB.Transaction
import AS.Reply

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
  errOrUpdate <- runDispatchCycle state cells DescendantsWithParent (userCommitSource uc) id
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
  result <- runEitherT $ evaluateHeader (state^.appSettings) evalHeader
  sendToOriginal uc $ case result of 
        Left e -> failureMessage mid $ generateErrorMessage e
        Right v -> ClientMessage mid $ ShowHeaderResult v

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
      updateDBWithCommit (state^.appSettings.graphDbAddress) conn src c
      broadcastSheetUpdate mid state $ sheetUpdateFromCommit c

handleSetLanguagesInRange :: MessageId -> ASUserClient -> ServerState -> ASLanguage -> ASRange -> IO ()
handleSetLanguagesInRange mid uc state lang rng = do 
  let inds = rangeToIndices rng
      conn = state^.dbConn
  cells <- catMaybes <$> getCells conn inds -- disregard cells that are empty
  let cellsWithLangsChanged = map (cellExpression.language .~ lang) cells
  errOrUpdate <- runDispatchCycle state cellsWithLangsChanged DescendantsWithParent (userCommitSource uc) id
  broadcastErrOrUpdate mid state uc errOrUpdate

-- | The user has pressed the "kill" button for an overlong operation;
-- look up the relevant thread, and kill it if it exists.
handleTimeout :: MessageId -> MVar ServerState -> IO ()
handleTimeout mid state = 
  modifyMVar_ state $ \curState -> do
    maybe (return ()) killThread $
      M.lookup mid (view threads curState)
    return $ curState & threads %~ (M.delete mid)