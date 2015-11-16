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

import AS.DB.API as DB

import AS.Util
import AS.Config.Settings

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

import Control.Exception (catch, SomeException)

-----------------------------------------------------------------------------------------------------------------------
-- Exposed functions

evaluateLanguage :: ASSheetId -> ASReference -> FormattedIndValMap -> ASExpression -> EitherTExec (Formatted ASValue)
evaluateLanguage sid curRef valuesMap xp@(Expression str lang) = catchEitherT $ do
  printWithTimeT "Starting eval code"
  let unformattedValuesMap = M.map orig valuesMap
      maybeError = possiblyShortCircuit sid unformattedValuesMap xp
  case maybeError of
    Just e -> return $ return e -- short-circuited, return this error
    Nothing -> case lang of
      Excel -> do 
        KE.evaluate str curRef valuesMap
        -- Excel needs current location and un-substituted expression, and needs the formatted values for
        -- loading the initial entities
      otherwise -> return <$> execEvalInLang sid lang xpWithValuesSubstituted -- didn't short-circuit, proceed with eval as usual
       where xpWithValuesSubstituted = insertValues sid unformattedValuesMap xp

-- no catchEitherT here for now, but that's because we're obsolescing Repl for now
evaluateLanguageRepl :: ASSheetId -> ASExpression -> EitherTExec ASValue
evaluateLanguageRepl sid (Expression str lang) = case lang of
  Python  -> KP.evaluateRepl sid str
  R       -> KR.evaluateRepl sid str
  SQL     -> KP.evaluateSqlRepl sid str
  OCaml   -> KO.evaluateRepl sid str

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
possiblyShortCircuit :: ASSheetId -> IndValMap -> ASExpression -> Maybe ASValue
possiblyShortCircuit sheetid valuesMap xp =
  let depRefs   = getDependencies sheetid xp -- :: [ASReference]
      depInds   = map refToIndices depRefs   -- :: [Maybe [ASIndex]]
      depInds'  = MB.catMaybes depInds       -- :: [[ASIndex]]
      depInds'' = concat depInds'            -- :: [ASIndex]
      lang = language xp
      values = map (valuesMap M.!) depInds'' in  -- :: [ASValue]
  if (length depInds == length depInds') -- if there were no #REF!'s
    then MB.listToMaybe $ MB.catMaybes $ map (\(i,v) -> case v of
      NoValue                  -> handleNoValueInLang lang i
      ve@(ValueError _ _ _ _)  -> handleErrorInLang lang ve
      vee@(ValueExcelError _)  -> handleErrorInLang lang vee
      otherwise                -> Nothing) (zip depInds'' values)
    else handleRefErrorInLang lang

-- | Nothing if it's OK to pass in NoValue, appropriate ValueError if not.
handleRefErrorInLang :: ASLanguage -> Maybe ASValue
handleRefErrorInLang _ = Just $ ValueError "Referencing cell that does not exist" "RefError" "" (-1)

-- | Nothing if it's OK to pass in NoValue, appropriate ValueError if not.
handleNoValueInLang :: ASLanguage -> ASIndex -> Maybe ASValue
handleNoValueInLang Excel _   = Nothing
handleNoValueInLang Python _  = Nothing
handleNoValueInLang R _       = Nothing
handleNoValueInLang _ cellRef = Just $ ValueError ("Reference cell " ++ (indexToExcel cellRef) ++ " is empty.") "RefError" "" (-1)
-- TDODO: replace (show cellRef) with the actual ref (e.g. C3) corresponding to it

handleErrorInLang :: ASLanguage -> ASValue -> Maybe ASValue
handleErrorInLang Excel _   = Nothing
handleErrorInLang _ err = Just err

execEvalInLang :: ASSheetId -> ASLanguage -> String -> EitherTExec ASValue
execEvalInLang sid lang = case lang of
  Python  -> KP.evaluate sid 
  R       -> KR.evaluate sid 
  SQL     -> KP.evaluateSql sid 
  OCaml   -> KO.evaluate sid