{-# LANGUAGE QuasiQuotes #-}
module AS.Kernels.Python.Eval where

import AS.Kernels.Common
import AS.Kernels.LanguageUtils
import AS.Kernels.Python.Pyfi

import AS.Parsing.Read

import AS.Types.Cell
import AS.Types.Eval
import AS.Types.Errors
import AS.Types.Sheets
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
evaluate :: ASSheetId -> String -> EitherTExec CompositeValue
evaluate sid "" = return $ CellValue NoValue
evaluate sid str = do
    validCode <- formatCode sid Python str
    if isDebug
        then lift $ writeExecFile Python validCode
        else return ()
    execWrappedCode validCode

evaluateRepl :: ASSheetId -> String -> EitherTExec CompositeValue
evaluateRepl sid "" = return $ CellValue NoValue
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
        else return $ CellValue NoValue

evaluateHeader :: ASSheetId -> String -> EitherTExec CompositeValue
evaluateHeader sid str = do
    lift $ writeHeaderFile sid Python str 
    -- appropriating repl code for this. technically more correct to leave this blank, 
    -- since isPrintable from the REPL code is a screwed up function, but it works well enough
    -- for now when we don't have a better way to give the user the direct output of the header
    -- eval. 
    (_, evalCode) <- lift $ formatCodeRepl sid Python str
    lift $ writeHeaderRecord Python evalCode
    -- perform eval, if there's something we actually need to return
    if (evalCode /= "" && str /= "")
        then execWrappedCode evalCode
        else return $ CellValue NoValue


-- | SQL
evaluateSql :: ASSheetId -> String -> EitherTExec CompositeValue
evaluateSql _ "" = return $ CellValue NoValue
evaluateSql sid str = do
    validCode <- formatCode sid SQL str
    if isDebug
        then lift $ writeExecFile SQL validCode
        else return ()
    execWrappedCode validCode

evaluateSqlRepl :: ASSheetId -> String -> EitherTExec CompositeValue
evaluateSqlRepl _ "" = return $ CellValue NoValue
evaluateSqlRepl sid str = evaluateSql sid str
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
