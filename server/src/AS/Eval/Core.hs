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
import AS.Parsing.Substitutions

import AS.Util
import AS.Config.Settings

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

import Control.Exception (catch, SomeException)

-----------------------------------------------------------------------------------------------------------------------
-- Exposed functions

evaluateLanguage :: ASReference -> ASSheetId -> RefValMap -> ASExpression -> EitherTExec ASValue
evaluateLanguage curRef sheetid valuesMap xp@(Expression str lang) = catchEitherT $ do
  printWithTimeT "Starting eval code"
  let maybeError = possiblyShortCircuit sheetid valuesMap xp
  case maybeError of
    Just e -> return e -- short-circuited, return this error
    Nothing -> case lang of
      Excel -> KE.evaluate str curRef valuesMap -- Excel needs current location and un-substituted expression
      otherwise -> execEvalInLang lang xpWithValuesSubstituted -- didn't short-circuit, proceed with eval as usual
       where xpWithValuesSubstituted = insertValues sheetid valuesMap xp

evaluateLanguageRepl :: ASExpression -> EitherTExec ASValue
evaluateLanguageRepl (Expression str lang) = catchEitherT $ case lang of
  Python  -> KP.evaluateRepl str
  R       -> KR.evaluateRepl str
  SQL     -> KP.evaluateSqlRepl str
  OCaml   -> KO.evaluateRepl str

-----------------------------------------------------------------------------------------------------------------------
-- Helpers

-- | Checks for potentially bad inputs (NoValue or ValueError) among the arguments passed in. If no bad inputs,
-- return Nothing. Otherwise, if there are errors that can't be dealt with, return appropriate ASValue error.
possiblyShortCircuit :: ASSheetId -> RefValMap -> ASExpression -> Maybe ASValue
possiblyShortCircuit sheetid valuesMap xp =
  let depIndices = concat $ map refToIndices $ getDependencies sheetid xp
      refs   = map IndexRef depIndices
      lang = language xp
      values = map (valuesMap M.!) $ refs in
  MB.listToMaybe $ MB.catMaybes $ map (\(r,v) -> case r of
    OutOfBounds -> Just $ ValueError ("Referencing cell out of bounds.") "RefError" "" (-1)
    _ -> case v of 
      NoValue                 -> handleNoValueInLang lang r
      ve@(ValueError _ _ _ _) -> handleErrorInLang lang ve
      otherwise               -> Nothing) (zip depIndices values)

-- | Nothing if it's OK to pass in NoValue, appropriate ValueError if not.
handleNoValueInLang :: ASLanguage -> ASIndex -> Maybe ASValue
handleNoValueInLang Excel _   = Nothing
handleNoValueInLang _ cellRef = Just $ ValueError ("Reference cell " ++ (indexToExcel cellRef) ++ " is empty.") "RefError" "" (-1)
-- TDODO: replace (show cellRef) with the actual ref (e.g. C3) corresponding to it

handleErrorInLang :: ASLanguage -> ASValue -> Maybe ASValue
handleErrorInLang Excel _   = Nothing
handleErrorInLang _ err = Just err

execEvalInLang :: ASLanguage -> String -> EitherTExec ASValue
execEvalInLang lang = case lang of
  Python  -> KP.evaluate
  R       -> KR.evaluate
  SQL     -> KP.evaluateSql
  OCaml   -> KO.evaluate


