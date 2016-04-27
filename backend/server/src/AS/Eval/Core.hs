{-# LANGUAGE QuasiQuotes #-}

module AS.Eval.Core where

import System.IO
import System.Process
import Control.Applicative
import Data.Text as T (unpack, pack)
import Database.Redis (Connection)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Exception (catch, SomeException)
import Data.Maybe hiding (fromJust)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.List.Split as LS
import Text.ParserCombinators.Parsec

import AS.Prelude
import AS.Util
import AS.Types.Cell
import AS.Types.CellProps
import AS.Types.Excel (indexToExcel)
import AS.Types.Excel hiding (dbConn)
import AS.Types.Eval
import AS.Types.EvalHeader
import AS.Types.Formats
import AS.Types.Graph
import AS.Types.Network
import AS.Types.Messages (MessageId)

import AS.Kernels.Python.Client as KP
import AS.Kernels.R.Client as KR
import AS.Kernels.Excel.Eval as KE
import AS.Dispatch.Expanding as DE
import AS.Eval.ColRangeHelpers
import AS.Parsing.Read
import AS.Parsing.Show
import AS.Parsing.Common
import AS.Parsing.Substitutions
import AS.Logging
import AS.Config.Settings
import qualified AS.DB.API as DB
import qualified AS.DB.Eval as DE
import qualified AS.DB.Graph as G

----------------------------------------------------------------------------------------------------
-- Exposed functions

-- #needsrefactor this function can be split into nicer composed parts
evaluateLanguage :: ServerState -> MessageId -> ASIndex -> EvalContext -> ASExpression 
                 -> DE.EvalChainFunc -> EitherTExec (Formatted EvalResult)
evaluateLanguage state mid idx@(Index sid _) ctx xp@(Expression str lang) f = catchEitherT $ do
  let conn = state^.dbConn
  -- Function from ExRef to string for interpolation
  let replaceFunc ref = lookUpRef state mid lang ctx (exRefToASRef sid ref) f
  case equalsSignsCheck xp of
    Left v@(ValueError _ _) -> return . return $ EvalResult (CellValue v) Nothing 
    Left (ValueS s) -> do 
      let xp' = Expression s lang 
      KE.evaluate conn (xp'^.expression) idx (ctx^.virtualCellsMap)
    -- If we match something as a literal, just evaluate it in Excel.
    Right xp' -> do
      -- The expression has exactly one = sign. Note that xp' has the stripped =.
      -- Get the new expression after interpolation as well as all of the Excel references in xp
      -- in one parsing fell swoop.
      (interpolatedXp, refs) <- lift $ getSubstitutedXpAndReferences replaceFunc xp'
      let depRefs = map (exRefToASRef sid) refs
      let depInds = concat <$> mapM (DE.refToIndicesWithContextDuringEval conn ctx) depRefs
      -- Check for potentially bad inputs (NoValue or ValueError) among the arguments passed in. 
      sc <- bimapEitherT' (Left . onRefToIndicesFailure) (onRefToIndicesSuccess ctx xp') depInds
      case sc of 
        Left v@(ValueError _ _) -> return . return $ EvalResult (CellValue v) Nothing 
        Left (ValueS s) -> do 
          let excelXp = Expression s lang 
          KE.evaluate conn (excelXp^.expression) idx (ctx^.virtualCellsMap)
        -- If we match something as a literal, just evaluate it in Excel.
        Right nonInterpolatedXp -> case lang of
          Excel -> KE.evaluate conn (nonInterpolatedXp^.expression) idx (ctx^.virtualCellsMap)
            -- Excel needs current location and un-substituted expression, and needs the formatted 
            -- values for loading the initial entities
          SQL -> do 
            code <- lift $ sqlToPythonCode state mid sid ctx nonInterpolatedXp f
            return <$> KP.evaluateSql mid sid code
          Python -> return <$> KP.evaluate mid sid (interpolatedXp^.expression)
          R -> return <$> KR.evaluate mid sid (interpolatedXp^.expression)

-- Python kernel now requires sheetid because each sheet now has a separate namespace against 
--  which evals are executed
evaluateHeader :: MessageId -> EvalHeader -> EitherTExec EvalResult
evaluateHeader mid evalHeader = 
  case lang of 
    Python -> KP.evaluateHeader mid sid str
    R      -> KR.evaluateHeader mid sid str
  where 
    sid  = evalHeader^.evalHeaderSheetId
    lang = evalHeader^.evalHeaderLang
    str  = evalHeader^.evalHeaderExpr

----------------------------------------------------------------------------------------------------
-- Interpolation

-- #mustrefactor IO String should be EitherTExec string
lookUpRef :: ServerState 
          -> MessageId 
          -> ASLanguage 
          -> EvalContext 
          -> ASReference 
          -> DE.EvalChainFunc 
          -> IO String
lookUpRef state mid lang context ref f = showValue lang <$> cv
  where cv = DE.referenceToCompositeValue state mid context ref f

insertValues :: ServerState 
             -> MessageId 
             -> ASSheetId 
             -> EvalContext 
             -> ASExpression 
             -> DE.EvalChainFunc 
             -> IO EvalCode
insertValues state mid sheetid ctx xp f = view expression <$> replacedXp
  where 
    replacedXp = replaceRefsIO replaceFunc xp
    toASRef = exRefToASRef sheetid
    replaceFunc ref = lookUpRef state mid (xp^.language) ctx (toASRef ref) f

-- | Converts SQL code to Python code for pandasql. It calls evalSql with 
-- the arguments of new expression and context. The new expression has ranges
-- replaced with datasetX, and the context is just a list of stringified 
-- "range-type" objects (such as dataframes). The evalSql function is imported
-- in the Python kernel on start.
sqlToPythonCode :: ServerState 
                -> MessageId 
                -> ASSheetId 
                -> EvalContext 
                -> ASExpression 
                -> DE.EvalChainFunc 
                -> IO EvalCode
sqlToPythonCode state mid sheetid ctx xp f = do 
  let exRefs = getExcelReferences xp
      toASRef = exRefToASRef sheetid
      tableRefs = map toASRef $ filter refIsSQLTable exRefs 
  datasetVals <- mapM (\ref -> lookUpRef state mid SQL ctx ref f) tableRefs
  let showRef ref = if refIsSQLTable ref 
                      then do 
                        let i = $fromJust (L.findIndex (ref==) exRefs)
                        return $ "dataset" ++ show i 
                      else lookUpRef state mid SQL ctx (toASRef ref) f
  newExp <- replaceRefsIO showRef xp
  let query = escape (newExp^.expression)
  -- print query
  let evalStmt = "evalSql(\"" ++ query ++ "\", " ++ (show datasetVals) ++ ")"
  return evalStmt

escape :: String -> String
escape xs = concatMap f xs 
  where f '\"' = "\\\""
        f x     = [x]

----------------------------------------------------------------------------------------------------
-- Helpers

refIsSQLTable :: ExRef -> Bool
refIsSQLTable (ExRangeRef _ _ _) = True 
refIsSQLTable (ExPointerRef _ _ _) =  True
refIsSQLTable _ = False

-- | This type represents an eval short-circuit; either we evaluate Right xp, or we just 
-- short-circuit and interpret Left val as an Excel literal. 
type ShortCircuitEval = Either ASValue ASExpression

-- | Makes sure that there is exactly one equal sign in the expression. If there are none, it 
-- immediately evaluates as a literal, and otherwise returns an error saying that there are too 
-- many equals signs. If there's exactly one equals sign, strip that = sign and return the new xp.
-- #expressionrefactor
equalsSignsCheck :: ASExpression -> ShortCircuitEval
equalsSignsCheck xp@(Expression str lang) = 
  case length evalLines of 
    0 -> Left $ ValueS str 
    1 -> case lang of 
      Excel -> Right xp 
      _ -> Right $ Expression (L.intercalate "\n" strippedLines) lang
    _ -> Left $ ValueError "Too many lines beginning with equals signs" "Eval error"
  where
    startsWithEqual = L.isPrefixOf "=" 
    stripEqual line = if startsWithEqual line then $tail line else line
    lines = LS.splitOn "\n" str
    evalLines = filter startsWithEqual lines
    strippedLines = map stripEqual lines

-- | Map over both failure and success.
bimapEitherT' :: Functor m => (e -> b) -> (a -> b) -> EitherT e m a -> EitherT e m b
bimapEitherT' f g (EitherT m) = EitherT (fmap h m) where
  h (Left e)  = Right (f e)
  h (Right a) = Right (g a)

catchEitherT :: EitherTExec (Formatted EvalResult) -> EitherTExec (Formatted EvalResult)
catchEitherT a = do
  result <- liftIO $ catchAny (runEitherT a) whenCaught
  case result of
    Left e -> left e
    Right e -> right e
    where 
      whenCaught :: SomeException -> IO (Either ASExecError (Formatted EvalResult))
      whenCaught e = return . Right $ Formatted (EvalResult cv Nothing) Nothing
        where cv = CellValue $ ValueError (show e) "StdErr"

-- | Checks for potentially bad inputs (NoValue or ValueError) among the arguments passed in. 
-- If no bad inputs, return Nothing. Otherwise, if there are errors that can't be dealt with, 
-- return the appropriate ASValue error.
-- #expressionrefactor
possiblyShortCircuit :: Connection -> ASSheetId -> EvalContext -> ASExpression 
                     -> EitherTExec ShortCircuitEval
possiblyShortCircuit conn sheetid ctx xp = case equalsSignsCheck xp of
  Right xp' -> do
    let depRefs = getDependencies sheetid xp' -- :: [ASReference]
    let depInds = concat <$> mapM (DE.refToIndicesWithContextDuringEval conn ctx) depRefs
    bimapEitherT' (Left . onRefToIndicesFailure) (onRefToIndicesSuccess ctx xp') depInds
  Left v -> return $ Left v

-- When eval's ref to indices fails, we want the error message to be in the actual cell. 
-- Possibly short circuit will do this for us, because the evaluateLanguage code bypasses 
-- eval in this case. 
onRefToIndicesFailure :: ASExecError -> ASValue
onRefToIndicesFailure PointerToNormalCell = 
  ValueError "Pointer to normal cell" "EvalError"
onRefToIndicesFailure IndexOfPointerNonExistant = 
  ValueError "Index of pointer doesn't exist" "EvalError"
onRefToIndicesFailure _ = 
  ValueError "Some eval error" "EvalError"

onRefToIndicesSuccess :: EvalContext -> ASExpression -> [ASIndex] -> ShortCircuitEval
onRefToIndicesSuccess ctx xp depInds = fmap (const xp) $ sequence $ flip map (zip depInds values) $ 
  \(i, v) -> case v of
    NoValue                 -> handleNoValueInLang xp i
    ve@(ValueError _ _)     -> handleErrorInLang xp ve
    otherwise               -> Right xp
    where
      lang = xp^.language
      values = map (maybe NoValue (view cellValue) . (`M.lookup` (ctx^.virtualCellsMap))) depInds

-- | Return the same expression if it's OK to pass in NoValue, 
-- or the appropriate ValueError.
handleNoValueInLang :: ASExpression -> ASIndex -> Either ASValue ASExpression
handleNoValueInLang xp@(Expression _ Excel) _   = Right xp
handleNoValueInLang xp@(Expression _ Python) _   = Right xp
handleNoValueInLang xp@(Expression _ R) _   = Right xp
handleNoValueInLang xp@(Expression _ SQL) _ = Right xp

handleErrorInLang :: ASExpression -> ASValue -> Either ASValue ASExpression
handleErrorInLang xp@(Expression _ Excel) _  = Right xp
handleErrorInLang _ v = Left v

----------------------------------------------------------------------------------------------------

