module AS.Dispatch.EvalHeader where

import Prelude

import AS.Types.Core
import AS.Eval.Core as R (evaluateHeader)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)

import AS.Util

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

-- this file is for future kernel-based repl methods

runEvalHeader :: ASSheetId -> ASExpression -> IO ASServerMessage
runEvalHeader sid xp = do
    let lang = language xp
    val <- runEitherT $ R.evaluateHeader sid xp
    return $ case val of 
        (Left e) -> ServerMessage EvaluateHeader (Failure $ generateErrorMessage e) (PayloadE e)
        (Right v) -> ServerMessage EvaluateHeader Success (PayloadValue v)