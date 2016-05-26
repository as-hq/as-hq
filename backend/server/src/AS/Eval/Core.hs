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
import AS.Types.User hiding (userId)
import AS.Types.Commits

import AS.LanguageDefs (trimWhitespace)

import AS.Kernels.Python.Client as KP
import AS.Kernels.R.Client as KR
import AS.Kernels.Excel.Eval as KE
import AS.Dispatch.Expanding as DE
import AS.Eval.ColRangeHelpers
import AS.Parsing.Read
import AS.Parsing.Show
import AS.Parsing.Common
import AS.Parsing.Substitutions
import AS.Parsing.References (getDependencies, exRefToASRef)
import AS.Logging
import AS.Config.Settings

import qualified AS.DB.API as DB
import qualified AS.DB.Eval as DE
import qualified AS.DB.Graph as G
import AS.DB.API (getOpenedSheets)

----------------------------------------------------------------------------------------------------
-- Exposed functions

-- #needsrefactor this function can be split into nicer composed parts
evaluateLanguage :: MessageContext ->
                    EvalContext -> 
                    ASIndex -> 
                    ASExpression -> 
                    DE.EvalChainFunc -> 
                    EitherTExec (Formatted EvalResult)
evaluateLanguage msgctx evalctx idx xp f = catchEitherT $ do
  let conn = msgctx^.dbConnection
  let sid = idx^.locSheetId
  let wid = messageWorkbookId msgctx
  let uid = msgctx^.userClient.userId
  let mid = msgctx^.messageId
  let lang = xp^.language
  -- Function from ExRef to string for interpolation
  sheets <- liftIO $ getOpenedSheets conn uid
  let replaceFunc ref = lookUpRef msgctx evalctx lang (exRefToASRef sid sheets ref) f
  case checkEvaluableExpression xp of
    Left v@(ValueError _ _) -> return . return $ EvalResult (CellValue v) Nothing 
    Left (ValueS s) -> do 
      let xp' = Expression s lang 
      liftIO $ KE.evaluate conn uid (xp'^.expression) idx (evalctx^.virtualCellsMap)
    -- If we match something as a literal, just evaluate it in Excel.
    Right xp' -> do
      -- The expression has exactly one = sign. Note that xp' has the stripped =.
      -- Get the new expression after interpolation as well as all of the Excel references in xp
      -- in one parsing fell swoop.
      (interpolatedXp, refs) <- lift $ getSubstitutedXpAndReferences replaceFunc xp'
      let depRefs = map (exRefToASRef sid sheets) refs
      let depInds = concat <$> mapM (DE.refToIndicesWithContextDuringEval conn evalctx) depRefs
      -- Check for potentially bad inputs (NoValue or ValueError) among the arguments passed in. 
      sc <- bimapEitherT' (Left . onRefToIndicesFailure) (onRefToIndicesSuccess evalctx xp') depInds
      case sc of 
        Left v@(ValueError _ _) -> return . return $ EvalResult (CellValue v) Nothing 
        Left (ValueS s) -> do 
          let excelXp = Expression s lang 
          liftIO $ KE.evaluate conn uid (excelXp^.expression) idx (evalctx^.virtualCellsMap)
        -- If we match something as a literal, just evaluate it in Excel.
        Right nonInterpolatedXp -> case lang of
          Excel -> liftIO $ KE.evaluate conn uid (nonInterpolatedXp^.expression) idx (evalctx^.virtualCellsMap)
            -- Excel needs current location and un-substituted expression, and needs the formatted 
            -- values for loading the initial entities
          SQL -> do 
            code <- lift $ sqlToPythonCode msgctx evalctx nonInterpolatedXp f
            return <$> KP.evaluateSql mid wid code
          Python -> return <$> KP.evaluate mid wid (interpolatedXp^.expression)
          R -> return <$> KR.evaluate mid wid (interpolatedXp^.expression)

-- Python kernel now requires sheetid because each sheet now has a separate namespace against 
--  which evals are executed
evaluateHeader :: MessageId -> EvalHeader -> EitherTExec EvalResult
evaluateHeader mid evalHeader = 
  case lang of 
    Python -> KP.evaluateHeader mid wid str
    R      -> KR.evaluateHeader mid wid str
  where 
    wid  = evalHeader^.evalHeaderWorkbookId
    lang = evalHeader^.evalHeaderLang
    str  = evalHeader^.evalHeaderExpr

----------------------------------------------------------------------------------------------------
-- Interpolation

lookUpRef :: MessageContext
          -> EvalContext 
          -> ASLanguage 
          -> ASReference 
          -> DE.EvalChainFunc 
          -> IO String
lookUpRef msgctx evalctx lang ref f = 
  showValue lang <$> DE.referenceToCompositeValue msgctx evalctx ref f

insertValues :: MessageContext
             -> EvalContext 
             -> ASExpression 
             -> DE.EvalChainFunc 
             -> IO EvalCode
insertValues msgctx evalctx xp f = do
  let uid   = msgctx^.userClient.userId
      conn  = msgctx^.dbConnection
      sid   = messageSheetId msgctx
  sheets <- getOpenedSheets conn uid
  let toASRef = exRefToASRef sid sheets
      replaceFunc ref = lookUpRef msgctx evalctx (xp^.language) (toASRef ref) f
  replacedXp <- replaceRefsIO replaceFunc xp
  return $ replacedXp^.expression

-- | Converts SQL code to Python code for pandasql. It calls evalSql with 
-- the arguments of new expression and context. The new expression has ranges
-- replaced with datasetX, and the context is just a list of stringified 
-- "range-type" objects (such as dataframes). The evalSql function is imported
-- in the Python kernel on start.
sqlToPythonCode :: MessageContext
                -> EvalContext 
                -> ASExpression 
                -> DE.EvalChainFunc 
                -> IO EvalCode
sqlToPythonCode msgctx evalctx xp f = do 
  let conn  = msgctx^.dbConnection
      sid   = messageSheetId msgctx
      uid   = msgctx^.userClient.userId
  sheets <- getOpenedSheets conn uid
  let exRefs = getExcelReferences xp
      toASRef = exRefToASRef sid sheets
      tableRefs = map toASRef $ filter refIsSQLTable exRefs 
  datasetVals <- forM tableRefs $ \ref -> lookUpRef msgctx evalctx SQL ref f
  let showRef :: ExRef -> IO String
      showRef ref = if refIsSQLTable ref 
                      then do 
                        let (Just i) = L.findIndex (ref==) exRefs
                        return $ "dataset" ++ show i 
                      else lookUpRef msgctx evalctx SQL (toASRef ref) f
  newExp <- replaceRefsIO showRef xp
  let query = show . catLines . trimWhitespace SQL $ newExp^.expression
  -- print query
  let evalStmt = "evalSql(" ++ query ++ ", " ++ show datasetVals ++ ")"
  return evalStmt

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
checkEvaluableExpression :: ASExpression -> ShortCircuitEval
checkEvaluableExpression xp = case filter isEvaluableLine (lines str) of 
  []  -> Left $ ValueS str
  _:[] -> Right $ case (xp^.language) of 
    Excel -> xp
    _     -> xp & expression .~ unlines (map stripEqual $ lines str)
  _   -> Left $ ValueError "Too many lines beginning with equals signs" "Eval error"
  where
    str = xp^.expression
    isEvaluableLine ('=':_) = True
    isEvaluableLine _       = False
    stripEqual ('=':xs) = xs
    stripEqual xs       = xs

-- | Map over both failure and success.
bimapEitherT' :: Functor m => (e -> b) -> (a -> b) -> EitherT e m a -> EitherT e m b
bimapEitherT' f g (EitherT m) = EitherT (fmap h m) where
  h (Left e)  = Right (f e)
  h (Right a) = Right (g a)

catchEitherT :: EitherTExec (Formatted EvalResult) -> EitherTExec (Formatted EvalResult)
catchEitherT a = do
  result <- liftIO $ catchAny (runEitherT a) $ \e -> 
    let cv = CellValue $ ValueError (show e) "StdErr"
    in return . Right $ Formatted (EvalResult cv Nothing) Nothing
  hoistEither result 

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

