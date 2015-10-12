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
-- | Exposed functions

evaluateLanguage :: ASExpression -> ASIndex -> M.Map ASIndex ASValue -> EitherTExec ASValue
evaluateLanguage xp ref mp = case xp of 
	Expression _ _ -> evalCode ref mp xp  
	Reference _ _ -> return $ evalRef ref mp xp

evaluateLanguageRepl :: ASExpression -> EitherTExec ASValue
evaluateLanguageRepl (Expression str lang) = case lang of
	Python 	-> KP.evaluateRepl str
	R 		-> KR.evaluateRepl str
	SQL 	-> KP.evaluateSqlRepl str
	OCaml 	-> KO.evaluateRepl str

-----------------------------------------------------------------------------------------------------------------------
-- | Helpers

evalCode :: ASIndex -> M.Map ASIndex ASValue -> ASExpression -> EitherTExec ASValue
evalCode ref values xp@(Expression _ lang) = do
	showTime "Starting eval code"
	let purified = insertValues (locSheetId ref) values xp 
	--printTimed $ "Final eval xp: " ++ (show finalXp)
	if lang == Excel
		then KE.evaluate purified ref values
		else execEvaluateLang lang purified


execEvaluateLang :: ASLanguage -> String -> EitherTExec ASValue
execEvaluateLang lang str = case lang of 
	Python 	-> KP.evaluate str
	R 		-> KR.evaluate str
	SQL 	-> KP.evaluateSql str
	OCaml 	-> KO.evaluate str

evalRef :: ASIndex -> M.Map ASIndex ASValue -> ASExpression ->  ASValue
evalRef loc dict (Reference (IndexRef l) (a, b)) = 
	case (dict M.! l) of
	  	(ValueL lst) -> case (lst L.!!b) of
	  		(ValueL row) -> (row L.!! a)
	  		otherwise -> (lst L.!!b)
	  	(ValueObject _ _) -> NoValue -- TODO implement direct object field reference
	  	otherwise -> dict M.!  (loc) -- current reference