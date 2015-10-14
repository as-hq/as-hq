module AS.Kernels.R where

import AS.Types.Core

import AS.Kernels.Common
import AS.Kernels.LanguageUtils

import AS.Parsing.In

import AS.Config.Settings
import AS.Util

import Foreign.C.String

import qualified Foreign.R as R
import Foreign.R.Type as R
import Foreign.R (SEXP, SEXPTYPE)
import Language.R.Instance as R
import Language.R as LR
import Language.R.QQ
import H.Prelude as H

import Control.Monad.IO.Class
-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Exposed functions

evaluate :: String -> EitherTExec ASValue
evaluate "" = return NoValue
evaluate str = do
    printWithTimeT "starting R eval"
    result <- lift $ execR str
    hoistEither $ parseValue R result

evaluateRepl :: String -> EitherTExec ASValue
evaluateRepl "" = return NoValue
evaluateRepl str = left ExecError -- TODO

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Helpers

-- TODO R monad?????! 

execR = return
--execR :: String -> IO String
--execR s = do
--    R.withEmbeddedR R.defaultConfig $ R.runRegion $ do
--        fmap (H.fromSEXP . R.cast R.SString) [r|
--library("rjson")
--library("jpeg")
--library("party")

--isError = FALSE
--result = tryCatch({
--eval(parse(s_hs))
--}, warning = function(w) {
--    # nothing here
--}, error = function(e) {
--    isError <<- TRUE
--    err = paste0("'error': \'Error: ", gsub("'",'"',e$message), "\'")
--    err_type = "'err_type': \'try-error\'"
--    position = "'position': -1" # TODO figure out line number of stacktrace in r
--    file = paste0("'file': ", "\'", "run.r", "\'") #TODO get blamed file
--    paste0("{", err, ", ", err_type, ", ", position, ", ", file, "}")
--}, finally = function() {
--    # nothing here
--})

-- # traceback()

--if (isError) cat(result) else cat(toJSON(result))
--eval(parse(text=s_hs)) |]