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

import Prelude()
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

import AS.Kernels.Python as KP
import AS.Kernels.R as KR
import AS.Kernels.Excel.Eval as KE
import AS.Kernels.OCaml as KO
import AS.Dispatch.Expanding as DE
import AS.Eval.ColRangeHelpers
import AS.Parsing.Read
import AS.Parsing.Show
import AS.Parsing.Common
import AS.Parsing.Substitutions
import AS.Logging
import AS.Config.Settings
import qualified AS.DB.API as DB
import qualified AS.DB.Graph as G

type EvalChainFunc = ServerState -> [ASCell] -> EvalContext -> EitherTExec EvalContext

-----------------------------------------------------------------------------------------------------------------------
-- Exposed functions

evaluateLanguage :: ServerState -> ASIndex -> EvalContext -> ASExpression -> EvalChainFunc -> EitherTExec (Formatted EvalResult)
evaluateLanguage state idx@(Index sid _) ctx xp@(Expression str lang) f = catchEitherT $ do
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
        pythonSqlCode <- lift $ sqlToPythonCode state sid ctx xp f
        return <$> KP.evaluateSql sid pythonSqlCode
      otherwise -> do 
        header <- lift $ DB.getEvalHeader conn sid lang
        xpWithValuesSubstituted <- lift $ insertValues state sid ctx xp f
        return <$> execEvalInLang header xpWithValuesSubstituted 
        -- didn't short-circuit, proceed with eval as usual

-- Python kernel now requires sheetid because each sheet now has a separate namespace against which evals are executed
evaluateHeader :: EvalHeader -> EitherTExec EvalResult
evaluateHeader evalHeader = 
  case lang of 
    Python -> KP.evaluateHeader sid str
    R      -> KR.evaluateHeader str
  where 
    sid  = evalHeader^.evalHeaderSheetId
    lang = evalHeader^.evalHeaderLang
    str  = evalHeader^.evalHeaderExpr

-- no catchEitherT here for now, but that's because we're obsolescing Repl for now. (Alex ~11/10)
-- DEPRECATED for now
--evaluateLanguageRepl :: String -> ASExpression -> EitherTExec CompositeValue
--evaluateLanguageRepl header (Expression str lang) = case lang of
--  Python  -> KP.evaluateRepl header str
--  R       -> KR.evaluateRepl str
--  SQL     -> KP.evaluateSqlRepl header str
--  OCaml   -> KO.evaluateRepl header str

-----------------------------------------------------------------------------------------------------------------------
-- Interpolation

-- #mustrefactor IO String should be EitherTExec string
lookUpRef :: ServerState -> ASLanguage -> EvalContext -> ASReference -> EvalChainFunc -> IO String
lookUpRef state lang context ref f = showValue lang <$> referenceToCompositeValue state context ref f

insertValues :: ServerState -> ASSheetId -> EvalContext -> ASExpression -> EvalChainFunc -> IO EvalCode
insertValues state sheetid ctx xp f = view expression <$> replaceRefsIO replacer xp
  where replacer ref = lookUpRef state (xp^.language) ctx (exRefToASRef sheetid ref) f

-- | We evaluate SQL expressions by converting them to Python code, and substituting it into a template
-- file that imports pysql and defines helper functions for SQL.  
-- Note: this can probably be significantly optimized. 
-- TODO clean up SQL mess
sqlToPythonCode :: ServerState -> ASSheetId -> EvalContext -> ASExpression -> EvalChainFunc -> IO EvalCode
sqlToPythonCode state sheetid ctx xp f = do 
  let exRefs = getExcelReferences xp
      matchedRefs = map (exRefToASRef sheetid) exRefs -- ASReferences found inside xp
  context <- mapM (\ref -> lookUpRef state SQL ctx ref f) matchedRefs
  let st = ["dataset"++(show i) | i<-[0..((L.length matchedRefs)-1)]]
      newExp = view expression $ replaceRefs (\el -> (L.!!) st ($fromJust (L.findIndex (el==) exRefs))) xp
      contextStmt = "setGlobals(" ++ show context ++ ")\n"
      evalStmt = "db(\'" ++ newExp ++ "\')"
  return $ contextStmt ++ evalStmt

-- used by lookUpRef
--  #mustrefactor IO CompositeValue should be EitherTExec CompositeValue
--  #mustrefactor why isn't this left IndexOfPointerNonExistant
referenceToCompositeValue :: ServerState -> EvalContext -> ASReference -> EvalChainFunc -> IO CompositeValue
referenceToCompositeValue _ ctx (IndexRef i) _ = return $ CellValue . view cellValue $ $valAt i $ ctx^.virtualCellsMap
referenceToCompositeValue _ ctx (PointerRef p) _ = do 
  let idx = pointerIndex p
  let mp = ctx^.virtualCellsMap
  let cell = $valAt idx mp
  case cell^.cellRangeKey of 
    Nothing -> $error "Pointer to normal expression!" 
    Just rKey -> do 
      case virtualRangeDescriptorAt ctx rKey of
        Nothing -> $error "Couldn't find range descriptor of coupled expression!"
        Just descriptor -> do 
          let indices = rangeKeyToIndices rKey
              cells  = map ((ctx^.virtualCellsMap) M.!) indices
              fatCell = FatCell cells descriptor
          printObj "REF TO COMPOSITE DESCRIPTOR: " descriptor
          return $ DE.recomposeCompositeValue fatCell
-- TODO: This is not the best way to do it: takes column cells, converts to indices, then converts back to values.....
referenceToCompositeValue _ ctx (ColRangeRef cr) _ = return $ Expanding . VList . M $ vals
  where
    indices = colRangeWithContextToIndicesRowMajor2D ctx cr
    -- The only case where the index is not in the virtualCellsMap is when the
    -- current dispatch created new cells in the bottom of a column whose
    -- colRange is being evaluated.
    indToVal ind = case M.member ind (ctx^.virtualCellsMap) of
                        True -> view cellValue $ $valAt ind (ctx^.virtualCellsMap)
                        False -> NoValue
    vals    = map (map indToVal) indices
referenceToCompositeValue _ ctx (RangeRef r) _ = return . Expanding . VList . M $ vals
  where
    indices = rangeToIndicesRowMajor2D r
    indToVal ind = view cellValue $ $valAt ind (ctx^.virtualCellsMap)
    vals    = map (map indToVal) indices
referenceToCompositeValue state ctx (TemplateRef t) f = 
  case t of 
    SampleExpr n idx -> $fromRight <$> (runEitherT $ do 
      -- Get all ancestors
      let conn = state^.dbConn
      ancRefs <- G.getAllAncestors $ indicesToAncestryRequestInput [idx]
      ancInds <- concat <$> mapM (refToIndices conn) ancRefs
      ancCells <- lift $ catMaybes <$> DB.getCellsWithContext conn ctx ancInds
      let ctxWithAncs = addCellsToContext ancCells ctx
      -- After adding ancestors to context, evaluate n times
      samples <- replicateM n $ evaluateNode state ctxWithAncs idx ancCells f
      return $ Expanding $ VList $ A samples)

-- Evaluate a node by running an evaluation function and extracting the answer from the context at the end. 
-- Assumes all ancestors are already in the context.
evaluateNode :: ServerState -> EvalContext -> ASIndex -> [ASCell] -> EvalChainFunc -> EitherTExec ASValue
evaluateNode state ctx idx ancestors f = do
  ctx' <- f state ancestors ctx
  return $ view cellValue . $valAt idx $ ctx'^.virtualCellsMap


----------------------------------------------------------------------------------------------------------------------
-- Reference conversions/lookups

-- Only used in conditional formatting.
-- TODO: timchu, 12/25/15. not sure if return $ colRange ... or  lift colRangeWithDB..
refToIndices :: Connection -> ASReference -> EitherTExec [ASIndex]
refToIndices conn (IndexRef i) = return [i]
refToIndices conn (ColRangeRef cr) = lift $ colRangeWithDBAndContextToIndices conn emptyContext cr
refToIndices conn (RangeRef r) = return $ rangeToIndices r
refToIndices conn (PointerRef p) = do
  let index = pointerIndex p
  cell <- lift $ DB.getCell conn index
  case cell of
    Nothing -> left IndexOfPointerNonExistant
    Just cell' -> case cell'^.cellRangeKey of
        Nothing -> left PointerToNormalCell
        Just rKey -> return $ rangeKeyToIndices rKey

-- This is the function we use to convert ref to indices for updating the map PRIOR TO eval. There are some cases where we don't flip a shit. 
-- For example, if the map currently has A1 as a normal expression, and we have @A1 somewhere downstream, we won't flip a shit, and instead expect that
-- by the time the pointer is evalled, A1 will have a coupled expression due to toposort. We flip a shit if it's not the case then. 
--  #record after PointerRef
refToIndicesWithContextBeforeEval :: Connection -> EvalContext -> ASReference -> IO [ASIndex]
refToIndicesWithContextBeforeEval conn _ (IndexRef i) = return [i]
refToIndicesWithContextBeforeEval conn _ (RangeRef r) = return $ rangeToIndices r
refToIndicesWithContextBeforeEval conn ctx (ColRangeRef r) = colRangeWithDBAndContextToIndices conn ctx r
refToIndicesWithContextBeforeEval conn ctx (PointerRef p) = do 
  let index = pointerIndex p
  case (M.lookup index $ ctx^.virtualCellsMap) of 
    Just c -> return $ maybe [] rangeKeyToIndices $ c^.cellRangeKey
    Nothing -> do
      cell <- DB.getCell conn index 
      case cell of
        Nothing -> return []
        Just cell' -> return $ maybe [] rangeKeyToIndices $ cell'^.cellRangeKey

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
execEvalInLang :: EvalHeader -> EvalCode -> EitherTExec EvalResult
execEvalInLang evalHeader = 
  case lang of
    Python  -> KP.evaluate sid
    R       -> KR.evaluate headerCode
    -- OCaml   -> KO.evaluate headerCode
  where 
    sid         = evalHeader^.evalHeaderSheetId
    lang        = evalHeader^.evalHeaderLang
    headerCode  = evalHeader^.evalHeaderExpr

-- #needsrefactor DRY this up
-- converts ref to indices using the evalContext, then the DB, in that order.
-- because our evalContext might contain information the DB doesn't (e.g. decoupling)
-- so in the pointer case, we need to check the evalContext first for changes that might have happened during eval
-- TODO: timchu. Only used in shortCircuitDuringEval. This could be renamed to be more clear.
--  #record after PointerRef
refToIndicesWithContextDuringEval :: Connection -> EvalContext -> ASReference -> EitherTExec [ASIndex]
refToIndicesWithContextDuringEval conn _ (IndexRef i) = return [i]
refToIndicesWithContextDuringEval conn _ (RangeRef r) = return $ rangeToIndices r
refToIndicesWithContextDuringEval conn ctx (ColRangeRef cr) = lift $ colRangeWithDBAndContextToIndices conn ctx cr
refToIndicesWithContextDuringEval conn ctx (PointerRef p) = do 
  let index = pointerIndex p
  case (M.lookup index $ ctx^.virtualCellsMap) of
    Just c -> maybe (left PointerToNormalCell) (return . rangeKeyToIndices) $ c^.cellRangeKey
    Nothing -> do
      cell <- lift $ DB.getCell conn index 
      case cell of
        Nothing -> left IndexOfPointerNonExistant
        Just cell' -> maybe (left PointerToNormalCell) (return . rangeKeyToIndices) $ cell'^.cellRangeKey
