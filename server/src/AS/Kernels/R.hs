module AS.Kernels.R where

import AS.Types.Core

import AS.Kernels.Common
import AS.Kernels.LanguageUtils

import AS.Parsing.In

import AS.Config.Settings
import AS.Util

--import qualified Foreign.R as R
--import Foreign.R (SEXP, SEXPTYPE)
--import Language.R.Instance as R
--import Language.R.QQ

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Exposed functions

evaluate :: String -> IO (Either ASExecError ASValue)
evaluate str = do
    if isDebug 
        then writeExecFile R str
        else return ()
    printTimed "starting R eval"
    result <- execR str
    return $ parseValue R result

evaluateRepl :: String -> IO (Either ASExecError ASValue)
evaluateRepl str = return $ Left ExecError -- TODO

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Helpers

-- TODO merge 
execR = return
--execR :: String -> R s String
--execR s = [r| print(s_hs) |]
