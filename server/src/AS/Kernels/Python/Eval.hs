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
evaluate :: String -> EitherTExec ASValue
evaluate "" = return NoValue
evaluate str = do
    validCode <- introspectCode Python str
    if isDebug
        then lift $ writeExecFile Python validCode
        else return ()
    execWrappedCode validCode

evaluateRepl :: String -> EitherTExec ASValue
evaluateRepl "" = return  NoValue
evaluateRepl str = do
    -- preprocess expression
    (recordCode, evalCode) <- lift $ introspectCodeRepl Python str
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

evaluateHeader :: String -> EitherTExec ASValue
evaluateHeader str = do
    -- preprocess expression
    lift $ writeHeaderFile Python str -- appropriating the repl record for 
    (_, evalCode) <- lift $ introspectCodeRepl Python str
    -- perform eval, if there's something we actually need to return
    if (evalCode /= "" && str /= "")
        then execWrappedCode evalCode
        else return NoValue


-- | SQL
evaluateSql :: String -> EitherTExec ASValue
evaluateSql "" = return NoValue
evaluateSql str = do
    validCode <- introspectCode SQL str
    if isDebug
        then lift $ writeExecFile SQL validCode
        else return ()
    execWrappedCode validCode

evaluateSqlRepl :: String -> EitherTExec ASValue
evaluateSqlRepl "" = return NoValue
evaluateSqlRepl str = evaluateSql str


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
