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
onParseSuccess :: String -> CompositeValue -> IO ()
onParseSuccess previousRecord (CellValue (ValueError _ _)) = writeReplRecord Python previousRecord
onParseSuccess _ v = return ()

onParseFailure :: String -> ASExecError -> IO ()
onParseFailure previousRecord x = writeReplRecord Python previousRecord

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
        else return (CellValue NoValue)

evaluateHeader :: ASSheetId -> String -> EitherTExec ASValue
evaluateHeader sid str = do
    lift $ writeHeaderFile sid Python str 
    -- appropriating repl code for this. technically more correct to leave this blank, 
    -- since isPrintable from th REPL code is a screwed up function, but it works well enough
    -- for now when we don't have a better way to give the user the direct output of the header
    -- eval. 
    (_, evalCode) <- lift $ formatCodeRepl sid Python str
    -- perform eval, if there's something we actually need to return
    if (evalCode /= "" && str /= "")
        then execWrappedCode evalCode
        else return NoValue


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

pyfiString :: String -> IO (Either CompositeValue String)
pyfiString evalStr = catch (Right <$> execString) whenCaught
    where
        execString = defVV (evalStr ++ pyString) ("Hello" :: String)
        whenCaught :: SomeException -> IO (Either CompositeValue String)
        whenCaught e = return . Left . CellValue $ ValueError (show e) "Syntax error."

pyString :: String
pyString = [str|
def export(x=None):
    return result
|]
