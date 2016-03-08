module AS.Eval.CondFormat where

import Prelude()
import AS.Prelude

import AS.DB.API as DB

import AS.Types.Cell
import AS.Types.CondFormat
import AS.Types.Errors
import AS.Types.Eval
import AS.Types.Formats
import AS.Types.Messages
import AS.Types.Network

import AS.Kernels.Python

import AS.Eval.Core
import AS.Parsing.Substitutions

import Data.List
import qualified Data.Map as M
import Database.Redis (Connection)
import Control.Monad (forM_, forever, (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (left)
import Control.Lens
import Data.Maybe
import AS.Logging


-- | Conditionally formats the cells based on the set of rules passed in -- all conditional formatting originally
-- in the cells go away. 
-- 
-- Note: The cells passed into conditionallyFormatCell should contain the most recent values for the cells.
-- In particular, if this was called through an eval, the values of ASCell should
-- agree with the values in EvalContext. If not, they should be the most recent values in the DB.
-- timchu, 12/17/15.
conditionallyFormatCells :: ServerState -> ASSheetId -> [ASCell] -> [CondFormatRule] -> EvalContext -> EvalChainFunc -> EitherTExec [ASCell]
conditionallyFormatCells state origSid cells rules ctx f = do
  let cells' = map (cellProps %~ clearCondFormatProps) cells
      transforms = map (ruleToCellTransform state origSid ctx f) rules
      transformsComposed = foldr (>=>) return transforms
  mapM transformsComposed cells'

-- #needsrefactor will eventually have to change ranges to refs in CondFormatRule
-- Requires that v is the most up to date ASValue at location l whenever this function is called.
ruleToCellTransform :: ServerState -> ASSheetId -> EvalContext -> EvalChainFunc -> CondFormatRule -> (ASCell -> EitherTExec ASCell)
ruleToCellTransform state sid ctx f (CondFormatRule _ rngs condMapConstructor) c = 
  let l = c^.cellLocation in 
  case find (flip rangeContainsIndex l) rngs of 
    Nothing -> return c
    Just rng -> do
      let tl = getTopLeft rng
          offset = getIndicesOffset tl l
          shiftXp = shiftExpression offset
          determineFormats v = case condMapConstructor of 
            BoolFormatMapConstructor boolCond props -> do 
              let mEvalLoc = shiftInd offset tl
              case mEvalLoc of 
                Nothing -> return []
                Just evalLoc -> do 
                  let shiftAndEvalExpr = (evaluateBoolExpression state evalLoc ctx f) . shiftXp 
                  shouldFormat <- checkBoolCond boolCond v shiftAndEvalExpr
                  return $ if shouldFormat then props else []
            LambdaFormatMapConstructor lambdaExpr -> do 
              let shiftedLambdaExpr = shiftXp (Expression lambdaExpr Python)
              formatResult <- evaluateFormatExpression state sid ctx shiftedLambdaExpr v f
              return $ case formatResult of 
                FormatSuccess props -> props
                _                   -> []
      conditionalFormats <- determineFormats $ c^.cellValue
      return $ c & cellProps %~ setCondFormatProps conditionalFormats

-- #needsrefactor this may not be the right place to put the below three functions.
-- Also, there are probably a ton of redundant calls to the DB -- we might be inserting 
-- into the EvalContext the same cell, pulled from the DB, over and over again in different calls to updatedContextForEval

-- | Takes in a context and an expression, and adds to the context all the cells referenced
-- in the expression that are not already in the context. This is the context that will be
-- used to evaluate said expression. 
updatedContextForEval :: ServerState -> ASSheetId -> EvalContext -> ASExpression -> EitherTExec EvalContext
updatedContextForEval state sid ctx xp = do
  let valMap = ctx^.virtualCellsMap
      deps = getDependencies sid xp 
      conn = state^.dbConn
  depInds <- concat <$> mapM (refToIndices conn) deps
  let depIndsToGet = filter (not . (flip M.member) valMap) depInds
  cells <- lift $ DB.getPossiblyBlankCells conn depIndsToGet
  let valMap' = insertMultiple valMap depIndsToGet cells
  -- #RoomForImprovement. Can probably use %~ and make this cleaner.
  return $ ctx & virtualCellsMap .~ valMap'

evaluateBoolExpression :: ServerState -> ASIndex -> EvalContext -> EvalChainFunc -> ASExpression -> EitherTExec ASValue
evaluateBoolExpression state evalLoc ctx f xp@(Expression str lang) = do
  let sid = evalLoc^.locSheetId
  ctx' <- updatedContextForEval state sid ctx xp
  (Formatted (EvalResult res _) _) <- evaluateLanguage state evalLoc ctx' xp f
  case res of
    Expanding expandingValue -> left $ CondFormattingError ("Tried to apply conditional formatting rule" ++ str ++ "but got ExpandingValue error with expandingValue:  " ++ show expandingValue)
    CellValue (ValueError msg _) -> do
      let errMsg = "Tried to apply conditional formatting rule " ++ str ++ " but got error" ++ (show msg) ++ ". "
      left $ CondFormattingError errMsg
    CellValue v -> return v

-- #needsrefactor should really have a typeclass that encompasses ASExpression and LambdaConditionExpr.
-- For now we're just shoehorning LambdaConditionExpr into an ASExpression. #expressiontypeclass
-- 
-- | Evaluates a Python function that returns a format or an error, rather than an ASValue. 
evaluateFormatExpression :: ServerState -> ASSheetId -> EvalContext -> ASExpression -> ASValue -> EvalChainFunc -> EitherTExec FormatResult
evaluateFormatExpression state sid ctx lambdaExpr v f = do
  let conn = state^.dbConn
  ctx' <- updatedContextForEval state sid ctx lambdaExpr
  lambdaExpr' <- lift $ insertValues state sid ctx' lambdaExpr f
  evaluateLambdaFormat sid lambdaExpr' v
