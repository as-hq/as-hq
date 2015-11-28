module AS.Handlers.Eval where

import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User

import AS.Dispatch.Core
import AS.Dispatch.Repl
import AS.Dispatch.EvalHeader

import AS.DB.API
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
  oldTags <- getPropsAt (map cellLocation cells)
  let cells' = map (\(c, ps) -> c { cellProps = ps }) (zip cells oldTags)
  msg' <- runDispatchCycle state cells' (userCommitSource uc)
  broadcastFiltered state uc msg'

handleEvalRepl :: ASUserClient -> ASPayload -> IO ()
handleEvalRepl uc (PayloadXp xp) = do
  let sid = userSheetId uc
  msg' <- runReplDispatch sid xp
  sendToOriginal uc msg'

handleEvalHeader :: ASUserClient -> ASPayload -> IO ()
handleEvalHeader uc (PayloadXp xp) = do
  let sid = userSheetId uc
  msg' <- runEvalHeader sid xp
  sendToOriginal uc msg'
