module AS.Handlers.Eval where

import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User
import AS.Types.Commits
import AS.Types.Eval

import AS.Dispatch.Core
import AS.Dispatch.Repl
import AS.Dispatch.EvalHeader

import AS.DB.API
import AS.DB.Eval
import AS.DB.Expanding
import AS.DB.Transaction
import AS.Reply

import Control.Concurrent

----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval handler

handleEval :: ASUserClient -> MVar ServerState -> ASExpression -> ASIndex -> IO ()
handleEval uc state xp ind  = do
  conn <- dbConn <$> readMVar state
  oldProps <- getPropsAt conn ind
  let cell = Cell ind xp NoValue oldProps
  errOrCommit <- runDispatchCycle state [cell] DescendantsWithParent (userCommitSource uc) id
  broadcastFiltered state uc $ makeReplyMessageFromErrOrCommit errOrCommit

-- not maintaining right now (Alex 12/28)
-- handleEvalRepl :: ASUserClient -> ASPayload -> IO ()
-- handleEvalRepl uc (PayloadXp xp) = do
--   let sid = userSheetId uc
--   msg' <- runReplDispatch sid xp
--   sendToOriginal uc msg'

handleEvalHeader :: ASUserClient -> MVar ServerState -> ASExpression -> IO ()
handleEvalHeader uc state xp@(Expression str lang) = do
  let sid = userSheetId uc
  conn <- dbConn <$> readMVar state
  setEvalHeader conn sid lang str
  msg' <- runEvalHeader sid xp
  sendToOriginal uc msg'

-- The user has said OK to the decoupling
-- We've stored the changed range keys and the last commit, which need to be used to modify DB
handleDecouple :: ASUserClient -> MVar ServerState -> IO ()
handleDecouple uc state = do 
  conn <- dbConn <$> readMVar state
  let src = userCommitSource uc
  mCommit <- getTempCommit conn src
  case mCommit of
    Nothing -> return ()
    Just c -> do
      updateDBWithCommit conn src c
      broadcastFiltered state uc $ makeReplyMessageFromCommit c

