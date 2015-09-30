{-# LANGUAGE QuasiQuotes #-}
module AS.Kernels.Python.Eval where

import AS.Types.Core hiding (str)

import AS.Kernels.Common
import AS.Kernels.LanguageUtils
import AS.Kernels.Python.Pyfi

import AS.Parsing.In

import AS.Util
import AS.Config.Settings

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Exposed functions

-- | python
evaluate :: String -> IO (Either ASExecError ASValue)
evaluate str = do
    code <- introspectCode Python str
    case code of 
        (Left e) -> return $ Left e
        (Right validCode) -> do
            if isDebug 
                then writeExecFile Python validCode
                else return ()
            result <- pyfiString validCode
            return $ parseValue Python result

evaluateRepl :: String -> IO (Either ASExecError ASValue)
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
    parsed <- if (evalCode == "")
        then return $ Right NoValue
        else do
            result <- pyfiString evalCode
            return $ parseValue Python result
    -- if error, undo the write to repl record
    case parsed of 
        (Left _)                        -> writeReplRecord Python replRecord
        (Right (ValueError _ _ _ _))    -> writeReplRecord Python replRecord
        otherwise -> return ()
    return parsed

-- | SQL
evaluateSql :: String -> IO (Either ASExecError ASValue)
evaluateSql str = do
    code <- introspectCode SQL str
    case code of
        (Left e) -> return $ Left e
        (Right validCode) -> do
            if isDebug 
                then writeExecFile SQL validCode
                else return ()
            result <- pyfiString validCode
            return $ parseValue Python result

evaluateSqlRepl :: String -> IO (Either ASExecError ASValue)
evaluateSqlRepl = evaluateSql 


----------------------------------------------------------------------------------------------------------------------------------------------
-- | helpers

pyfiString :: String -> IO String
pyfiString evalStr = defVV (evalStr ++ pyString) ("Hello" :: String)

pyString :: String
pyString = [str|
def export(x=1):
    return repr(result)
|]