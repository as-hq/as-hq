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

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Exposed functions

evaluate :: String -> EitherTExec ASValue
evaluate "" = return NoValue
evaluate str = do
    if isDebug 
        then lift $ writeExecFile R str
        else return ()
    printWithTimeT "starting R eval"
    result <- execR str
    hoistEither $ parseValue R result

evaluateRepl :: String -> EitherTExec ASValue
evaluateRepl "" = return NoValue
evaluateRepl str = left ExecError -- TODO

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Helpers

-- TODO merge 
execR = return
--execR :: String -> R s String
--execR s = [r| print(s_hs) |]
