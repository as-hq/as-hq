{-# LANGUAGE QuasiQuotes #-}
module AS.Kernels.Python.Eval where

import AS.Types.Core hiding (str)

import AS.Kernels.Common
import AS.Kernels.LanguageUtils
import AS.Kernels.Python.Pyfi

import AS.Parsing.Read

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
evaluate :: String -> EitherTExec CompositeValue
evaluate "" = return $ CellValue NoValue
evaluate str = do
    validCode <- introspectCode Python str
    if isDebug
        then lift $ writeExecFile Python validCode
        else return ()
    execWrappedCode validCode

evaluateRepl :: String -> EitherTExec CompositeValue
evaluateRepl "" = return $ CellValue NoValue
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
        else return (CellValue NoValue)

-- | SQL
evaluateSql :: String -> EitherTExec CompositeValue
evaluateSql "" = return $ CellValue NoValue
evaluateSql str = do
    validCode <- introspectCode SQL str
    if isDebug
        then lift $ writeExecFile SQL validCode
        else return ()
    execWrappedCode validCode

evaluateSqlRepl :: String -> EitherTExec CompositeValue
evaluateSqlRepl "" = return $ CellValue NoValue
evaluateSqlRepl str = evaluateSql str


----------------------------------------------------------------------------------------------------------------------------------------------
-- | helpers

execWrappedCode :: String -> EitherTExec CompositeValue
execWrappedCode evalCode = do
    result <- lift $ pyfiString evalCode
    case result of 
      Right result' -> hoistEither $ parseValue Python result'
      Left e -> return e

pyfiString :: String -> IO (Either ASValue String)
pyfiString evalStr = catch (Right <$> execString) whenCaught
    where
        execString = defVV (evalStr ++ pyString) ("Hello" :: String)
        whenCaught = (\e -> return . Left $ ValueError (show e) "SyntaxError" "" 0) :: (SomeException -> IO (Either ASValue String))

pyString :: String
pyString = [str|
def export(x=1):
    return repr(result)
|]
