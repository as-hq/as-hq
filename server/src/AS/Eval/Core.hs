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

-----------------------------------------------------------------------------------------------------------------------
-- | Exposed functions

evaluateLanguage :: ASExpression -> ASLocation -> M.Map ASLocation ASValue -> IO (Either ASExecError ASValue)
evaluateLanguage xp loc mp = case xp of 
	Expression _ _ -> evalCode (locSheetId loc) mp xp  
	Reference _ _ -> return . Right $ evalRef loc mp xp

evaluateLanguageRepl :: ASExpression -> IO (Either ASExecError ASValue)
evaluateLanguageRepl (Expression str lang) = case lang of
	Python 	-> KP.evaluateRepl str
	R 		-> KR.evaluateRepl str
	SQL 	-> KP.evaluateSqlRepl str
	OCaml 	-> KO.evaluateRepl str

-----------------------------------------------------------------------------------------------------------------------
-- | Helpers

evalCode :: ASSheetId -> M.Map ASLocation ASValue -> ASExpression -> IO (Either ASExecError ASValue)
evalCode sheetid values xp@(Expression str lang) = do
	printTimed "Starting eval code"
	let purified = insertValues sheetid values xp 
	--printTimed $ "Final eval xp: " ++ (show finalXp)
	execEvaluateLang lang purified
	

execEvaluateLang :: ASLanguage -> String -> IO (Either ASExecError ASValue)
execEvaluateLang lang str = case lang of 
	Python 	-> KP.evaluate str
	Excel 	-> KE.evaluate str
	R 		-> KR.evaluate str
	SQL 	-> KP.evaluateSql str
	OCaml 	-> KO.evaluate str

evalRef :: ASLocation -> M.Map ASLocation ASValue -> ASExpression ->  ASValue
evalRef loc dict (Reference l (a, b)) = 
	case (dict M.! l) of
	  	(ValueL lst) -> case (lst L.!!b) of
	  		(ValueL row) -> (row L.!! a)
	  		otherwise -> (lst L.!!b)
	  	(ValueObject _ _) -> NoValue -- TODO implement direct object field reference
	  	otherwise -> dict M.! loc -- current reference