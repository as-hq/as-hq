module AS.Dispatch.Repl where

import Prelude

import AS.Types.Core
import AS.Eval.Core as R
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)


-- this file is for future kernel-based repl methods

runReplDispatch :: MVar ServerState -> ASExpression -> IO ASServerMessage
runReplDispatch state xp = do
    val <- R.evalCodeRepl xp 
    return $ ServerMessage EvaluateRepl Success (PayloadLangValue (LangValue val (language xp)))