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

evaluateLanguage :: ASExpression -> ASIndex -> M.Map ASReference ASValue -> EitherTExec ASValue
evaluateLanguage xp ref mp = case xp of 
	Expression _ _ -> evalCode (locSheetId ref) mp xp  
	Reference _ _ -> return $ evalRef ref mp xp

evaluateLanguageRepl :: ASExpression -> EitherTExec ASValue
evaluateLanguageRepl (Expression str lang) = case lang of
	Python 	-> KP.evaluateRepl str
	R 		-> KR.evaluateRepl str
	SQL 	-> KP.evaluateSqlRepl str
	OCaml 	-> KO.evaluateRepl str

-----------------------------------------------------------------------------------------------------------------------
-- | Helpers

evalCode :: ASSheetId -> M.Map ASReference ASValue -> ASExpression -> EitherTExec ASValue
evalCode sheetid values xp@(Expression _ lang) = do
	showTime "Starting eval code"
	let purified = insertValues sheetid values xp 
	--printTimed $ "Final eval xp: " ++ (show finalXp)
	execEvaluateLang lang purified
	

execEvaluateLang :: ASLanguage -> String -> EitherTExec ASValue
execEvaluateLang lang str = case lang of 
	Python 	-> KP.evaluate str
	Excel 	-> KE.evaluate str
	R 		-> KR.evaluate str
	SQL 	-> KP.evaluateSql str
	OCaml 	-> KO.evaluate str

evalRef :: ASIndex -> M.Map ASReference ASValue -> ASExpression ->  ASValue
evalRef loc dict (Reference l (a, b)) = 
	case (dict M.! l) of
	  	(ValueL lst) -> case (lst L.!!b) of
	  		(ValueL row) -> (row L.!! a)
	  		otherwise -> (lst L.!!b)
	  	(ValueObject _ _) -> NoValue -- TODO implement direct object field reference
	  	otherwise -> dict M.! (IndexRef loc) -- current reference
