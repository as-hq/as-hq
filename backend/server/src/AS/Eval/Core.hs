{-# LANGUAGE QuasiQuotes #-}
module AS.Eval.Core where

import Prelude()
import AS.Prelude

import System.IO
import System.Process
import Control.Applicative

import Data.Maybe
import Data.Text as T (unpack, pack)

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T

import AS.Types.Eval
import AS.Types.EvalHeader
import AS.Types.Excel (indexToExcel)
import AS.Types.Formats
import AS.Types.Cell
import AS.Types.CellProps
import AS.Types.Network
import AS.Util

import AS.Kernels.LanguageUtils
import AS.Kernels.Python as KP
import AS.Kernels.R as KR
import AS.Kernels.Excel.Eval as KE
import AS.Kernels.OCaml as KO

import AS.Parsing.Read
import AS.Parsing.Show
import AS.Parsing.Common
import AS.Parsing.Substitutions

import AS.DB.Eval
import AS.DB.API

import AS.Logging
import AS.Config.Settings

import Database.Redis (Connection)

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Exception (catch, SomeException)

import Database.Redis (Connection)

-----------------------------------------------------------------------------------------------------------------------
-- Exposed functions

evaluateLanguage :: ServerState -> ASIndex -> EvalContext -> ASExpression -> EitherTExec (Formatted EvalResult)
evaluateLanguage state idx@(Index sid _) ctx xp@(Expression str lang) = catchEitherT $ do
  printWithTimeT "Starting eval code"
  let conn = state^.dbConn
  maybeShortCircuit <- possiblyShortCircuit conn sid ctx xp
  case maybeShortCircuit of
    Just e -> return . return $ EvalResult (CellValue e) Nothing -- short-circuited, return this error
    Nothing -> case lang of
      Excel -> KE.evaluate conn str idx (ctx^.virtualCellsMap)
        -- Excel needs current location and un-substituted expression, and needs the formatted values for
        -- loading the initial entities
      SQL -> do 
        pythonSqlCode <- lift $ sqlToPythonCode conn sid ctx xp
        return <$> KP.evaluateSql (state^.appSettings.pyKernelAddress) sid pythonSqlCode
      otherwise -> do 
        header <- lift $ getEvalHeader conn sid lang
        xpWithValuesSubstituted <- lift $ insertValues conn sid ctx xp
        return <$> execEvalInLang (state^.appSettings) header xpWithValuesSubstituted 
        -- didn't short-circuit, proceed with eval as usual

-- no catchEitherT here for now, but that's because we're obsolescing Repl for now. (Alex ~11/10)
-- DEPRECATED for now
--evaluateLanguageRepl :: String -> ASExpression -> EitherTExec CompositeValue
--evaluateLanguageRepl header (Expression str lang) = case lang of
--  Python  -> KP.evaluateRepl header str
--  R       -> KR.evaluateRepl str
--  SQL     -> KP.evaluateSqlRepl header str
--  OCaml   -> KO.evaluateRepl header str

-- Python kernel now requires sheetid because each sheet now has a separate namespace against which evals are executed
evaluateHeader :: AppSettings -> EvalHeader -> EitherTExec EvalResult
evaluateHeader sett evalHeader = 
  case lang of 
    Python -> KP.evaluateHeader (sett^.pyKernelAddress) sid str
    R      -> KR.evaluateHeader str
  where 
    sid  = evalHeader^.evalHeaderSheetId
    lang = evalHeader^.evalHeaderLang
    str  = evalHeader^.evalHeaderExpr

-----------------------------------------------------------------------------------------------------------------------
-- Helpers

-- | Map over both failure and success.
bimapEitherT' :: Functor m => (e -> b) -> (a -> b) -> EitherT e m a -> EitherT e m b
bimapEitherT' f g (EitherT m) = EitherT (fmap h m) where
  h (Left e)  = Right (f e)
  h (Right a) = Right (g a)

catchEitherT :: EitherTExec (Formatted EvalResult) -> EitherTExec (Formatted EvalResult)
catchEitherT a = do
  result <- liftIO $ catch (runEitherT a) whenCaught
  case result of
    Left e -> left e
    Right e -> right e
    where 
      whenCaught :: SomeException -> IO (Either ASExecError (Formatted EvalResult))
      whenCaught e = return . Right $ Formatted (EvalResult cv Nothing) Nothing
        where cv = CellValue $ ValueError (show e) "StdErr"

-- | Checks for potentially bad inputs (NoValue or ValueError) among the arguments passed in. If no bad inputs,
-- return Nothing. Otherwise, if there are errors that can't be dealt with, return appropriate ASValue error.
possiblyShortCircuit :: Connection -> ASSheetId -> EvalContext -> ASExpression -> EitherTExec (Maybe ASValue)
possiblyShortCircuit conn sheetid ctx xp = do 
  let depRefs = getDependencies sheetid xp -- :: [ASReference]
  let depInds = concat <$> mapM (refToIndicesWithContextDuringEval conn ctx) depRefs
  bimapEitherT' (Just . onRefToIndicesFailure) (onRefToIndicesSuccess ctx xp) depInds

-- When eval's ref to indices fails, we want the error message to be in the actual cell. Possibly short circuit will
-- do this for us, because the evaluateLanguage code bypasses eval in this case. 
onRefToIndicesFailure :: ASExecError -> ASValue
onRefToIndicesFailure PointerToNormalCell = ValueError "Pointer to normal cell" "EvalError"
onRefToIndicesFailure IndexOfPointerNonExistant = ValueError "Index of pointer doesn't exist" "EvalError"
onRefToIndicesFailure _ = ValueError "Some eval error" "EvalError"

onRefToIndicesSuccess :: EvalContext -> ASExpression -> [ASIndex] -> Maybe ASValue
onRefToIndicesSuccess ctx xp depInds = listToMaybe $ catMaybes $ flip map (zip depInds values) $ \(i, v) -> case v of
  NoValue                 -> handleNoValueInLang lang i
  ve@(ValueError _ _)     -> handleErrorInLang lang ve
  otherwise               -> Nothing
  where
    lang           = xp^.language
    values         = map (maybe NoValue (view cellValue) . (`M.lookup` (ctx^.virtualCellsMap))) depInds


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

-- Python kernel now requires sheetid because each sheet now has a separate namespace against which evals are executed
execEvalInLang :: AppSettings -> EvalHeader -> EvalCode -> EitherTExec EvalResult
execEvalInLang settings evalHeader = 
  case lang of
    Python  -> KP.evaluate (settings^.pyKernelAddress) sid
    R       -> KR.evaluate headerCode
    -- OCaml   -> KO.evaluate headerCode
  where 
    sid         = evalHeader^.evalHeaderSheetId
    lang        = evalHeader^.evalHeaderLang
    headerCode  = evalHeader^.evalHeaderExpr
