module AS.Dispatch.EvalHeader where

import Prelude

import AS.Types.Cell
import AS.Types.Messages
import AS.Types.Eval

import AS.Eval.Core as R (evaluateHeader)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)

import AS.Util

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

-- this file is for future kernel-based repl methods

-- #needsrefactor shouldn't return the value, not the message
runEvalHeader :: ASSheetId -> ASExpression -> IO ASServerMessage
runEvalHeader sid xp = do
    let lang = language xp
    val <- runEitherT $ R.evaluateHeader xp
    return $ case val of 
        Left e -> failureMessage $ generateErrorMessage e
        Right v -> ServerMessage $ ShowHeaderResult v