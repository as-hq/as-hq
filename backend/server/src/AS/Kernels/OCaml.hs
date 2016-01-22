module AS.Kernels.OCaml where

import AS.Prelude
import Prelude()

import AS.Types.Cell
import AS.Types.Eval
import AS.Types.Errors

import AS.Kernels.Internal
import AS.Kernels.LanguageUtils

import AS.Parsing.Read

import AS.Config.Settings
import AS.Logging

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Exposed functions

--  DEPRECATED until we actually support OCaml (anand 1/6/15)
evaluate :: String -> String -> EitherTExec EvalResult
evaluate _ _ = $undefined
--evaluate _ "" = return $ CellValue NoValue
--evaluate _ str = do
    --if isDebug 
    --    then lift $ writeExecFile OCaml str
    --    else return ()
    --printWithTimeT "starting OCaml eval"
    --result <- lift $ execOcaml
    --hoistEither $ parseValue OCaml result

--  DEPRECATED until we actually support OCaml (anand 1/6/15)
evaluateRepl :: String -> String -> EitherTExec EvalResult
evaluateRepl _ _ = $undefined
--evaluateRepl _ "" = return $ CellValue NoValue
--evaluateRepl _ str = left ExecError -- TODO
----------------------------------------------------------------------------------------------------------------------------------------------
-- | Helpers

--execOcaml :: IO String
--execOcaml = do
--    args <- getRunnerArgs OCaml
--    file <- getRunFile OCaml
--    terminalCmd <- addCompileCmd OCaml $ formatRunArgs OCaml (getRunnerCmd OCaml) file args
--    res <- evalShell OCaml terminalCmd
--    return res

--runReplFile :: IO String
--runReplFile = do
--    args <- getRunnerArgs OCaml
--    file <- getRunReplFile OCaml
--    let terminalCmd = formatRunArgs OCaml (getRunnerCmdRepl OCaml) file args
--    res <- eval terminalCmd OCaml
--    return res