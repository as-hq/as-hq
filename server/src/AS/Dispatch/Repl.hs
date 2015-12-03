module AS.Dispatch.Repl where

import Prelude

import AS.Types.Cell
import AS.Types.Messages 
import AS.Types.Eval

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
        (Right (CellValue v)) -> ServerMessage EvaluateRepl Success (PayloadReplValue $ ReplValue v lang)
        (Left e) -> ServerMessage EvaluateRepl (Failure $ generateErrorMessage e) (PayloadReplValue $ ReplValue (execErrorToValueError e) lang)
        _ ->  ServerMessage EvaluateRepl (Failure "Could not return composite value in REPL.") (PayloadReplValue $ ReplValue (execErrorToValueError APIError) lang)