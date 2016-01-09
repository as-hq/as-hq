module AS.Dispatch.EvalHeader where

import Prelude()
import AS.Prelude

import AS.Types.Cell
import AS.Types.Messages
import AS.Types.Eval
import AS.Types.EvalHeader
import AS.Types.Network

import AS.Eval.Core as R (evaluateHeader)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Lens

import AS.Util

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Lens

-- this file is for future kernel-based repl methods

-- #needsrefactor should return the value, not the message
runEvalHeader :: AppSettings -> EvalHeader -> IO ClientMessage
runEvalHeader settings evalHeader = do
    let lang = evalHeader^.evalHeaderLang
    val <- runEitherT $ R.evaluateHeader settings evalHeader
    return $ case val of 
        Left e -> failureMessage $ generateErrorMessage e
        Right v -> ClientMessage $ ShowHeaderResult v