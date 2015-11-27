module AS.Kernels.OCaml where

import AS.Types.Cell

import AS.Kernels.Common
import AS.Kernels.LanguageUtils

import AS.Parsing.Read

import AS.Config.Settings
import AS.Util

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Exposed functions

evaluate :: ASSheetId -> String -> EitherTExec CompositeValue
evaluate _ "" = return $ CellValue NoValue
evaluate _ str = do
    if isDebug 
        then lift $ writeExecFile OCaml str
        else return ()
    printWithTimeT "starting OCaml eval"
    result <- lift $ execOcaml
    hoistEither $ parseValue OCaml result

evaluateRepl :: ASSheetId -> String -> EitherTExec CompositeValue
evaluateRepl _ "" = return $ CellValue NoValue
evaluateRepl _ str = left ExecError -- TODO
----------------------------------------------------------------------------------------------------------------------------------------------
-- | Helpers

execOcaml :: IO String
execOcaml = do
    args <- getRunnerArgs OCaml
    file <- getRunFile OCaml
    terminalCmd <- addCompileCmd OCaml $ formatRunArgs OCaml (getRunnerCmd OCaml) file args
    res <- evalShell OCaml terminalCmd
    return res

--runReplFile :: IO String
--runReplFile = do
--    args <- getRunnerArgs OCaml
--    file <- getRunReplFile OCaml
--    let terminalCmd = formatRunArgs OCaml (getRunnerCmdRepl OCaml) file args
--    res <- eval terminalCmd OCaml
--    return res