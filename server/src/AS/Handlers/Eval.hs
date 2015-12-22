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

handleEval :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleEval uc state payload  = do
  let cells = case payload of 
                PayloadCL cells' -> cells'
  -- The PayloadCL is sort of a misnomer; it's only really being used as a wrapper around the
  -- expression to evaluate and the location of evaluation. In particular, the value passed in the cells
  -- are irrelevant, and there are no tags passed in, so we have to get the tags from the database
  -- manually. 
  conn <- dbConn <$> readMVar state
  oldTags <- getPropsAt conn (map cellLocation cells)
  let cells' = map (\(c, ps) -> c { cellProps = ps }) (zip cells oldTags)
  errOrCommit <- runDispatchCycle state cells' DescendantsWithParent (userCommitSource uc) id
  broadcastFiltered state uc $ makeReplyMessageFromErrOrCommit errOrCommit

handleEvalRepl :: ASUserClient -> ASPayload -> IO ()
handleEvalRepl uc (PayloadXp xp) = do
  let sid = userSheetId uc
  msg' <- runReplDispatch sid xp
  sendToOriginal uc msg'

handleEvalHeader :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleEvalHeader uc state (PayloadXp xp@(Expression str lang)) = do
  let sid = userSheetId uc
  conn <- dbConn <$> readMVar state
  setEvalHeader conn sid lang str
  msg' <- runEvalHeader sid xp
  sendToOriginal uc msg'

-- The user has said OK to the decoupling
-- We've stored the changed range keys and the last commit, which need to be used to modify DB
handleDecouple :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleDecouple uc state payload = do 
  conn <- dbConn <$> readMVar state
  let src = userCommitSource uc
  mCommit <- getTempCommit conn src
  case mCommit of
    Nothing -> return ()
    Just c -> do
      updateDBWithCommit conn src c
      broadcastFiltered state uc $ makeReplyMessageFromCommit c

