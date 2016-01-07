module AS.Handlers.Eval where

import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User
import AS.Types.Commits
import AS.Types.Eval
import AS.Types.Network

import AS.Dispatch.Core
--import AS.Dispatch.Repl
import AS.Dispatch.EvalHeader

import AS.DB.API
import AS.DB.Eval
import AS.DB.Expanding
import AS.DB.Transaction
import AS.Reply

import Control.Concurrent
import Control.Lens hiding ((.=))

----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval handler

handleEval :: ASUserClient -> MVar ServerState -> [EvalInstruction] -> IO ()
handleEval uc state evalInstructions  = do
  let xps  = map evalXp evalInstructions
      inds = map evalLoc evalInstructions
  conn <- view dbConn <$> readMVar state
  oldProps <- mapM (getPropsAt conn) inds
  let cells = map (\(xp, ind, props) -> Cell ind xp NoValue props Nothing Nothing) $ zip3 xps inds oldProps
  errOrUpdate <- runDispatchCycle state cells DescendantsWithParent (userCommitSource uc) id
  broadcastErrOrUpdate state uc errOrUpdate

-- not maintaining right now (Alex 12/28)
-- handleEvalRepl :: ASUserClient -> ASPayload -> IO ()
-- handleEvalRepl uc (PayloadXp xp) = do
--   let sid = userSheetId uc
--   msg' <- runReplDispatch sid xp
--   sendToOriginal uc msg'

handleEvalHeader :: ASUserClient -> ServerState -> ASExpression -> IO ()
handleEvalHeader uc state xp@(Expression str lang) = do
  let sid = userSheetId uc
  setEvalHeader (state^.dbConn) sid lang str
  msg' <- runEvalHeader (state^.appSettings) sid xp
  sendToOriginal uc msg'

-- The user has said OK to the decoupling
-- We've stored the changed range keys and the last commit, which need to be used to modify DB
handleDecouple :: ASUserClient -> MVar ServerState -> IO ()
handleDecouple uc mstate = do 
  state <- readMVar mstate
  let conn = state^.dbConn
      src = userCommitSource uc
  mCommit <- getTempCommit conn src
  case mCommit of
    Nothing -> return ()
    Just c -> do
      updateDBWithCommit (state^.appSettings.graphDbAddress) conn src c
      broadcastSheetUpdate mstate $ sheetUpdateFromCommit c

