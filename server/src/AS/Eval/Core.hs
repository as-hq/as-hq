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

evalCode :: ASSheetId -> RefValMap -> ASExpression -> EitherTExec ASValue
evalCode sheetid valuesMap xp@(Expression _ lang) = do
	printWithTimeT "Starting eval code"
	checkForValueErrors sheetid valuesMap xp
	let xpWithValuesSubstituted = insertValues sheetid valuesMap xp 
	execEvalInLang lang xpWithValuesSubstituted	

checkForValueErrors :: ASSheetId -> RefValMap -> ASExpression -> EitherTExec ()
checkForValueErrors sheetid valuesMap xp = do 
	let depIndices = getDependencies sheetid xp 
	let refs   = fmap IndexRef depIndices
	let values = map (valuesMap M.!) $ refs
	flip mapM_ (zip depIndices values) (\(di, v) -> case v of 
		NoValue            -> left $ DBNothingException [di]
		ValueError _ _ _ _ -> left $ EvaluationError "referenced cell with error in it"
		otherwise    -> return ())

execEvalInLang :: ASLanguage -> String -> EitherTExec ASValue
execEvalInLang lang = case lang of 
	Python 	-> KP.evaluate
	Excel 	-> KE.evaluate
	R 		-> KR.evaluate
	SQL 	-> KP.evaluateSql
	OCaml 	-> KO.evaluate

-- Deprecated. 

--evalRef :: ASIndex -> RefValMap -> ASExpression ->  ASValue
--evalRef loc dict (Reference l (a, b)) = 
--	case (dict M.! l) of
--	  	(ValueL lst) -> case (lst L.!!b) of
--	  		(ValueL row) -> (row L.!! a)
--	  		otherwise -> (lst L.!!b)
--	  	(ValueObject _ _) -> NoValue -- TODO implement direct object field reference
--	  	otherwise -> dict M.! (IndexRef loc) -- current reference
