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

import AS.Parsing.Read
import AS.Parsing.Show
import AS.Parsing.Common
import AS.Parsing.Substitutions

import AS.DB.API as DB

import AS.Util
import AS.Config.Settings

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

import Control.Exception (catch, SomeException)

-----------------------------------------------------------------------------------------------------------------------
-- Exposed functions

evaluateLanguage :: ASIndex -> ASSheetId -> ValMap -> ASExpression -> EitherTExec (Formatted CompositeValue)
evaluateLanguage curRef sheetid valuesMap xp = 
  let maybeError = possiblyShortCircuit sheetid valuesMap xp
      lang = xpLanguage xp
      str = xpString xp
      xpWithValuesSubstituted = insertValues sheetid valuesMap xp
      unformattedValuesMap = M.map orig valuesMap
  in catchEitherT $ do
    printWithTimeT $ "Starting eval code: " ++ xpWithValuesSubstituted 
    case maybeError of
      Just e -> return $ CellValue e -- short-circuited, return this error
      Nothing -> case lang of
        Excel -> KE.evaluate str curRef unformattedValuesMap -- Excel needs current location and un-substituted expression
        otherwise -> execEvalInLang lang xpWithValuesSubstituted -- didn't short-circuit, proceed with eval as usual

evaluateLanguageRepl :: ASExpression -> EitherTExec CompositeValue
evaluateLanguageRepl (Expression str lang) = catchEitherT $ case lang of
  Python  -> KP.evaluateRepl str
  R       -> KR.evaluateRepl str
  SQL     -> KP.evaluateSqlRepl str
  OCaml   -> KO.evaluateRepl str

-- #incomplete needs stuff for R (Alex 11/13)
evaluateHeader :: ASSheetId -> ASExpression -> EitherTExec ASValue
evaluateHeader sid (Expression str lang) = case lang of 
  Python -> KP.evaluateHeader sid str
  R      -> KR.evaluateHeader sid str

-----------------------------------------------------------------------------------------------------------------------
-- Helpers

-- [Maybe [ASIndex]] --> [[ Maybe ASIndex ]]

-- | Checks for potentially bad inputs (NoValue or ValueError) among the arguments passed in. If no bad inputs,
-- return Nothing. Otherwise, if there are errors that can't be dealt with, return appropriate ASValue error.
possiblyShortCircuit :: ASSheetId -> ValMap -> ASExpression -> Maybe ASValue
possiblyShortCircuit sheetid valuesMap xp =
  let depRefs       = getDependencies sheetid xp -- :: [ASReference]
      depSets       = map refToIndices depRefs   -- :: [Maybe [ASIndex]]
      isOutOfBounds = any MB.isNothing depSets
      depInds       = concat $ filterNothing depSets
      lang          = xpLanguage xp
      values        = map (valuesMap M.!) $ depInds
  in if isOutOfBounds 
    then Just $ ValueError "Referencing cell out of bounds." "RefError"
    else MB.listToMaybe $ MB.catMaybes $ flip map (zip depInds values) $ \(i, v) -> case v of
      CellValue NoValue                 -> handleNoValueInLang lang i
      CellValue ve@(ValueError _ _)     -> handleErrorInLang lang ve
      otherwise                         -> Nothing 

-- | Nothing if it's OK to pass in NoValue, appropriate ValueError if not.
handleNoValueInLang :: ASLanguage -> ASIndex -> Maybe ASValue
handleNoValueInLang Excel _   = Nothing
handleNoValueInLang Python _  = Nothing
handleNoValueInLang R _       = Nothing
handleNoValueInLang _ cellRef = Just $ ValueError ("Reference cell " ++ (indexToExcel cellRef) ++ " is empty.") "RefError"
-- TDODO: replace (show cellRef) with the actual ref (e.g. C3) corresponding to it

handleErrorInLang :: ASLanguage -> ASValue -> Maybe ASValue
handleErrorInLang Excel _  = Nothing
handleErrorInLang _ err = Just err

execEvalInLang :: ASLanguage -> String -> EitherTExec CompositeValue
execEvalInLang lang = case lang of
  Python  -> KP.evaluate
  R       -> KR.evaluate
  SQL     -> KP.evaluateSql
  OCaml   -> KO.evaluate