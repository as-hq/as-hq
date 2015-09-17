module AS.Dispatch.Repl where

import Prelude

import AS.Types
import AS.Eval.Core as R
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)


-- this file is for future kernel-based repl methods

runReplDispatch :: ASUser -> MVar ServerState -> ASMessage -> IO ASMessage
runReplDispatch user state (Message uid action _ (PayloadXp xp)) = do
    val <- R.evalCodeRepl xp 
    return $ Message uid action Success (PayloadLangValue (LangValue val (language xp)))