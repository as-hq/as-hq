module AS.Dispatch.Repl where

import Prelude

import AS.Types.Core
import AS.Eval.Core as R (evaluateLanguageRepl)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)

import AS.Util

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

-- this file is for future kernel-based repl methods

runReplDispatch :: ASSheetId -> ASExpression -> IO ASServerMessage
runReplDispatch sid xp = do
    let lang = language xp
    val <- runEitherT $ R.evaluateLanguageRepl sid xp 
    return $ case val of 
        (Left e) -> ServerMessage EvaluateRepl (Failure $ generateErrorMessage e) (PayloadE e)
        (Right v) -> ServerMessage EvaluateRepl Success (PayloadReplValue $ ReplValue v lang)