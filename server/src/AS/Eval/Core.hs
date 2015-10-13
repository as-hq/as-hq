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

evaluateLanguageRepl :: ASExpression -> EitherTExec ASValue
evaluateLanguageRepl (Expression str lang) = case lang of
	Python 	-> KP.evaluateRepl str
	R 		-> KR.evaluateRepl str
	SQL 	-> KP.evaluateSqlRepl str
	OCaml 	-> KO.evaluateRepl str

-----------------------------------------------------------------------------------------------------------------------
-- Helpers

evaluateLanguage :: ASSheetId -> RefValMap -> ASExpression -> EitherTExec ASValue
evaluateLanguage sheetid valuesMap xp@(Expression _ lang) = do
	printWithTimeT "Starting eval code"
	let maybeError = possiblyShortCircuit sheetid valuesMap xp
	case maybeError of 
		Just e -> return e -- short-circuited, return this error
		Nothing -> execEvalInLang lang xpWithValuesSubstituted -- didn't short-circuit, proceed with eval as usual
       where xpWithValuesSubstituted = insertValues sheetid valuesMap xp 

-- | Checks for potentially bad inputs (NoValue or ValueError) among the arguments passed in. If no bad inputs, 
-- return Nothing. Otherwise, if there are errors that can't be dealt with, return appropriate ASValue error.
possiblyShortCircuit :: ASSheetId -> RefValMap -> ASExpression -> Maybe ASValue
possiblyShortCircuit sheetid valuesMap xp = 
	let depIndices = getDependencies sheetid xp 
	    refs   = map IndexRef depIndices
	    lang = language xp
	    values = map (valuesMap M.!) $ refs in 
	MB.listToMaybe $ MB.catMaybes $ map (\(r,v) -> case v of 
		NoValue                 -> handleNoValueInLang lang r
		ve@(ValueError _ _ _ _) -> handleErrorInLang lang ve 
		otherwise               -> Nothing) (zip depIndices values)

-- | Nothing if it's OK to pass in NoValue, appropriate ValueError if not.
handleNoValueInLang :: ASLanguage -> ASIndex -> Maybe ASValue
handleNoValueInLang Excel _   = Nothing
handleNoValueInLang _ cellRef = Just $ ValueError ("Reference cell " ++ (show cellRef) ++ " is empty.") RefError "" (-1)
-- TDODO: replace (show cellRef) with the actual ref (e.g. C3) corresponding to it

handleErrorInLang :: ASLanguage -> ASValue -> Maybe ASValue
handleErrorInLang Excel _   = Nothing
handleErrorInLang _ err = Just err

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
