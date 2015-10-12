{-# LANGUAGE QuasiQuotes #-}
module AS.Eval.Core where

import Prelude
import System.IO           
import System.Process  
import Control.Applicative
 
import qualified Data.Maybe as MB
import Data.Time.Clock
import Data.Text as T (unpack,pack)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T

import AS.Types.Core hiding (str)

import AS.Kernels.LanguageUtils
import AS.Kernels.Python.Eval as KP
import AS.Kernels.R as KR
import AS.Kernels.Excel.Eval as KE
import AS.Kernels.OCaml as KO

import AS.Parsing.In
import AS.Parsing.Out
import AS.Parsing.Common

import AS.Util
import AS.Config.Settings

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

-----------------------------------------------------------------------------------------------------------------------
-- Exposed functions

evalReplExpression :: ASExpression -> EitherTExec ASValue
evalReplExpression (Expression str lang) = case lang of
	Python 	-> KP.evaluateRepl str
	R 		-> KR.evaluateRepl str
	SQL 	-> KP.evaluateSqlRepl str
	OCaml 	-> KO.evaluateRepl str

-----------------------------------------------------------------------------------------------------------------------
-- Helpers

evalCode :: ASReference -> ASSheetId -> RefValMap -> ASExpression -> EitherTExec ASValue
evalCode ref sheetid values xp@(Expression str lang) = do
	printWithTimeT "Starting eval code"
	let xpWithValuesSubstituted = insertValues sheetid values xp 
	if lang == Excel
		then KE.evaluate str ref values
		else execEvalInLang lang xpWithValuesSubstituted	

execEvalInLang :: ASLanguage -> String -> EitherTExec ASValue
execEvalInLang lang = case lang of 
	Python 	-> KP.evaluate
	R 		-> KR.evaluate
	SQL 	-> KP.evaluateSql
	OCaml 	-> KO.evaluate


