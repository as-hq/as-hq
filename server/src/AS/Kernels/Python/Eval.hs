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
----------------------------------------------------------------------------------------------------------------------------------------------
-- | Exposed functions

-- | python
evaluate :: String -> IO (Either ASExecError ASValue)
evaluate "" = return $ Right NoValue
evaluate str = do
    code <- introspectCode Python str
    case code of 
        (Left e) -> return $ Left e
        (Right validCode) -> do
            if isDebug 
                then writeExecFile Python validCode
                else return ()
            execWrappedCode validCode

evaluateRepl :: String -> IO (Either ASExecError ASValue)
evaluateRepl "" = return $ Right NoValue
evaluateRepl str = do
    -- preprocess expression
    (recordCode, evalCode) <- introspectCodeRepl Python str
    if isDebug
        then writeReplFile Python evalCode
        else return ()
    -- write record
    replRecord <- getReplRecord Python
    writeReplRecord Python (replRecord ++ "\n" ++ recordCode)
    -- perform eval
    parsed <- if (evalCode == emptyExpression)
        then return $ Right NoValue
        else execWrappedCode evalCode
    -- if error, undo the write to repl record
    case parsed of 
        (Left _)                        -> writeReplRecord Python replRecord
        (Right (ValueError _ _ _ _))    -> writeReplRecord Python replRecord
        otherwise -> return ()
    return parsed

-- | SQL
evaluateSql :: String -> IO (Either ASExecError ASValue)
evaluateSql "" = return $ Right NoValue
evaluateSql str = do
    code <- introspectCode SQL str
    case code of
        (Left e) -> return $ Left e
        (Right validCode) -> do
            if isDebug 
                then writeExecFile SQL validCode
                else return ()
            execWrappedCode validCode

evaluateSqlRepl :: String -> IO (Either ASExecError ASValue)
evaluateSqlRepl "" = return $ Right NoValue
evaluateSqlRepl str = evaluateSql str


----------------------------------------------------------------------------------------------------------------------------------------------
-- | helpers

execWrappedCode :: String -> IO (Either ASExecError ASValue)
execWrappedCode evalCode = do
    result <- pyfiString evalCode
    return $ case result of 
        (Left SyntaxError) -> Left SyntaxError
        (Right s) -> parseValue Python s

pyfiString :: String -> IO (Either ASExecError String)
pyfiString evalStr = catch (fmap Right execString) whenCaught
    where 
        execString = defVV (evalStr ++ pyString) ("Hello" :: String)
        whenCaught = (\e -> return $ Left SyntaxError) :: (SomeException -> IO (Either ASExecError String))

pyString :: String
pyString = [str|
def export(x=1):
    return repr(result)
|]