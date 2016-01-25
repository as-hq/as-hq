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

import Control.Concurrent
import Control.Lens hiding ((.=))
import Control.Monad.Trans.Either

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
<<<<<<< HEAD
handleDecouple :: MessageId -> ASUserClient -> MVar ServerState -> IO ()
handleDecouple mid uc mstate = do 
  state <- readMVar mstate
=======
handleDecouple :: ASUserClient -> ServerState -> IO ()
handleDecouple uc state = do 
>>>>>>> 44ef029... Removed extraneous uses of MVar Serverstate, propagated throughout code.
  let conn = state^.dbConn
      src = userCommitSource uc
  mCommit <- getTempCommit conn src
  case mCommit of
    Nothing -> return ()
    Just c -> do
      updateDBWithCommit (state^.appSettings.graphDbAddress) conn src c
<<<<<<< HEAD
      broadcastSheetUpdate mid mstate $ sheetUpdateFromCommit c
=======
      broadcastSheetUpdate state $ sheetUpdateFromCommit c
>>>>>>> 44ef029... Removed extraneous uses of MVar Serverstate, propagated throughout code.

