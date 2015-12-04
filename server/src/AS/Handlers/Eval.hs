module AS.Handlers.Eval where

import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User
import AS.Types.Commits

import AS.Dispatch.Core
import AS.Dispatch.Repl
import AS.Dispatch.EvalHeader

import AS.DB.API
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
  oldTags <- getPropsAt (map cellLocation cells)
  let cells' = map (\(c, ps) -> c { cellProps = ps }) (zip cells oldTags)
  msg' <- runDispatchCycle state cells' False (userCommitSource uc)
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

-- The user has said OK to the decoupling
-- We've stored the changed range keys and the last commit, which need to be used to modify DB
handleDecouple :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleDecouple cl state payload = do 
  conn <- dbConn <$> readMVar state
  let src = userCommitSource cl
  mCommit <- getTempCommit conn src
  mRangeKeys <- getRangeKeysChanged conn src
  case mCommit of
    Nothing -> return ()
    Just c  -> do 
      case mRangeKeys of 
        Nothing -> return ()
        Just rangeKeysChanged -> do 
          concat <$> (mapM (decouple conn) rangeKeysChanged)
          updateDBAfterEval conn src c
          let msg = ServerMessage Update Success (PayloadCL (after c))
          broadcastFiltered state cl msg

