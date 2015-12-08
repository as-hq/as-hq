{-# LANGUAGE QuasiQuotes #-}
module AS.Eval.Core where

import Prelude
import System.IO
import System.Process
import Control.Applicative

import Data.Maybe
import Data.Text as T (unpack, pack)

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T

import AS.Types.Eval
import AS.Types.Excel (indexToExcel)
import AS.Types.Cell

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

import AS.Logging
import AS.Config.Settings

import Database.Redis (Connection)

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

import Control.Monad.IO.Class (liftIO)
import Control.Exception (catch, SomeException)

import Database.Redis (Connection)

-----------------------------------------------------------------------------------------------------------------------
-- Exposed functions

evaluateLanguage :: Connection -> ASIndex -> EvalContext -> ASExpression -> EitherTExec (Formatted CompositeValue)
evaluateLanguage conn idx@(Index sid _) ctx xp@(Expression str lang) = catchEitherT $ do
  printWithTimeT "Starting eval code"
  maybeShortCircuit <- possiblyShortCircuit sid ctx xp
  header <- lift $ DB.getEvalHeader conn sid lang
  xpWithValuesSubstituted <- lift $ insertValues conn sid ctx xp
  case maybeShortCircuit of
    Just e -> return . return . CellValue $ e -- short-circuited, return this error
    Nothing -> case lang of
      Excel -> do 
        KE.evaluate str idx (contextMap ctx)
        -- Excel needs current location and un-substituted expression, and needs the formatted values for
        -- loading the initial entities
      otherwise -> return <$> execEvalInLang header lang xpWithValuesSubstituted -- didn't short-circuit, proceed with eval as usual
evaluateLanguage _ _ _ (Coupled _ _ _ _) = left WillNotEvaluate

-- no catchEitherT here for now, but that's because we're obsolescing Repl for now. (Alex ~11/10)
evaluateLanguageRepl :: String -> ASExpression -> EitherTExec CompositeValue
evaluateLanguageRepl header (Expression str lang) = case lang of
  Python  -> KP.evaluateRepl header str
  R       -> KR.evaluateRepl str
  SQL     -> KP.evaluateSqlRepl header str
  OCaml   -> KO.evaluateRepl header str

evaluateHeader :: ASExpression -> EitherTExec CompositeValue
evaluateHeader (Expression str lang) = case lang of 
  Python -> KP.evaluateHeader str
  R      -> KR.evaluateHeader str

-----------------------------------------------------------------------------------------------------------------------
-- Helpers

catchEitherT :: EitherTExec (Formatted CompositeValue) -> EitherTExec (Formatted CompositeValue)
catchEitherT a = do
  result <- liftIO $ catch (runEitherT a) whenCaught
  case result of
    Left e -> left e
    Right e -> right e
    where 
      whenCaught :: SomeException -> IO (Either ASExecError (Formatted CompositeValue))
      whenCaught e = return . Right $ Formatted (CellValue $ ValueError (show e) "StdErr") Nothing

-- | Checks for potentially bad inputs (NoValue or ValueError) among the arguments passed in. If no bad inputs,
-- return Nothing. Otherwise, if there are errors that can't be dealt with, return appropriate ASValue error.
possiblyShortCircuit :: ASSheetId -> EvalContext -> ASExpression -> EitherTExec (Maybe ASValue)
possiblyShortCircuit sheetid ctx xp = do 
  let depRefs        = getDependencies sheetid xp -- :: [ASReference]
  depInds <- concat <$> mapM (refToIndicesWithContextDuringEval ctx) depRefs   -- :: [Maybe [ASIndex]]
  let lang           = xpLanguage xp
      values         = map (cellValue . ((contextMap ctx) M.!)) depInds
  return $ listToMaybe $ catMaybes $ flip map (zip depInds values) $ \(i, v) -> case v of
    NoValue                 -> handleNoValueInLang lang i
    ve@(ValueError _ _)     -> handleErrorInLang lang ve
    otherwise               -> Nothing 

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

execEvalInLang :: String -> ASLanguage -> String -> EitherTExec CompositeValue
execEvalInLang header lang = case lang of
  Python  -> KP.evaluate header 
  R       -> KR.evaluate header 
  SQL     -> KP.evaluateSql header 
  OCaml   -> KO.evaluate header