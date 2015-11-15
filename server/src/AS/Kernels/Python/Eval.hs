{-# LANGUAGE QuasiQuotes #-}
module AS.Kernels.Python.Eval where

import AS.Types.Core hiding (str)

import AS.Kernels.Common
import AS.Kernels.LanguageUtils
import AS.Kernels.Python.Pyfi

import AS.Parsing.In

import AS.Util
import AS.Config.Settings

import Control.Exception (catch, SomeException)

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
----------------------------------------------------------------------------------------------------------------------------------------------
-- | Exposed functions

-- | Helper for evaluateRepl
onParseSuccess :: String -> ASValue -> IO ()
onParseSuccess replRecord v@(ValueError _ _ _ _) = writeReplRecord Python replRecord
onParseSuccess _ v = return ()

onParseFailure :: String -> ASExecError -> IO ()
onParseFailure replRecord x = writeReplRecord Python replRecord

-- | python
evaluate :: ASSheetId -> String -> EitherTExec ASValue
evaluate sid "" = return NoValue
evaluate sid str = do
    validCode <- formatCode sid Python str
    if isDebug
        then lift $ writeExecFile Python validCode
        else return ()
    execWrappedCode validCode

evaluateRepl :: ASSheetId -> String -> EitherTExec ASValue
evaluateRepl sid "" = return NoValue
evaluateRepl sid str = do
    -- preprocess expression
    (recordCode, evalCode) <- lift $ formatCodeRepl sid Python str
    if isDebug
        then lift $ writeReplFile Python evalCode
        else return ()
    -- write record
    replRecord <- lift $ getReplRecord Python
    lift $ writeReplRecord Python (replRecord ++ "\n" ++ recordCode)
    -- perform eval, if there's something we actually need to return
    if (evalCode /= "")
        then do 
            let parsed = execWrappedCode evalCode
            -- rollback to previous repl state if eval failed
            lift $ eitherT (onParseFailure replRecord) (onParseSuccess replRecord) parsed
            parsed
        else return NoValue

evaluateHeader :: ASSheetId -> String -> EitherTExec ASValue
evaluateHeader sid str = do
    -- preprocess expression
    lift $ writeHeaderFile sid Python str -- appropriating the repl record for 
    -- appropriating repl code for this. technically more correct to leave this blank, 
    -- since isPrintable from th REPL code is a screwed up function, but it works well enough
    -- for now when we don't have a better way to give the user the direct output of the header
    -- eval. 
    (_, evalCode) <- lift $ formatCodeRepl sid Python str
    printDebugT "evalCode" evalCode
    -- perform eval, if there's something we actually need to return
    if (evalCode /= "" && str /= "")
        then execWrappedCode evalCode
        else return NoValue


-- | SQL
evaluateSql :: ASSheetId -> String -> EitherTExec ASValue
evaluateSql _ "" = return NoValue
evaluateSql sid str = do
    validCode <- formatCode sid SQL str
    if isDebug
        then lift $ writeExecFile SQL validCode
        else return ()
    execWrappedCode validCode

evaluateSqlRepl :: ASSheetId -> String -> EitherTExec ASValue
evaluateSqlRepl _ "" = return NoValue
evaluateSqlRepl sid str = evaluateSql sid str

----------------------------------------------------------------------------------------------------------------------------------------------
-- | helpers

execWrappedCode :: String -> EitherTExec ASValue
execWrappedCode evalCode = do
    result <- lift $ pyfiString evalCode
    case result of 
      Right result' -> hoistEither $ parseValue Python result'
      Left e -> return e

pyfiString :: String -> IO (Either ASValue String)
pyfiString evalStr = catch (fmap Right execString) whenCaught
    where
        execString = defVV (evalStr ++ pyString) ("Hello" :: String)
        whenCaught = (\e -> return . Left $ ValueError (show e) "SyntaxError" "" 0) :: (SomeException -> IO (Either ASValue String))

pyString :: String
pyString = [str|
def export(x=1):
    return repr(result)
|]
