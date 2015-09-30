module AS.Kernels.OCaml where

import AS.Types.Core

import AS.Kernels.Common
import AS.Kernels.LanguageUtils

import AS.Parsing.In

import AS.Config.Settings
import AS.Util

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Exposed functions

evaluate :: String -> IO (Either ASExecError ASValue)
evaluate str = do
    if isDebug 
        then writeExecFile OCaml str
        else return ()
    printTimed "starting OCaml eval"
    result <- execOcaml
    return $ parseValue OCaml result

evaluateRepl :: String -> IO (Either ASExecError ASValue)
evaluateRepl str = return $ Left ExecError -- TODO

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