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
onParseSuccess replRecord (ValueError _ _ _ _) = writeReplRecord Python replRecord
onParseSuccess _ _ = return ()

onParseFailure :: String -> ASExecError -> IO ()
onParseFailure replRecord _ = writeReplRecord Python replRecord

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
    -- perform eval
    let parsed = if (evalCode == emptyExpression)
        then return NoValue
        else execWrappedCode evalCode
    lift $ eitherT (onParseFailure replRecord) (onParseSuccess replRecord) parsed
    parsed

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
    result <- pyfiString evalCode
    hoistEither $ parseValue Python result

pyfiString :: String -> EitherTExec String
pyfiString evalStr = EitherT $ catch (fmap Right execString) whenCaught
    where
        execString = defVV (evalStr ++ pyString) ("Hello" :: String)
        whenCaught = (\e -> return $ Left SyntaxError) :: (SomeException -> IO (Either ASExecError String))

pyString :: String
pyString = [str|
def export(x=1):
    return repr(result)
|]
