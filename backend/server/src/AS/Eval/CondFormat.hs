module AS.Eval.CondFormat where

import AS.Prelude

import AS.DB.API as DB
import AS.DB.Eval

import AS.Types.Cell
import AS.Types.CondFormat
import AS.Types.Errors
import AS.Types.Eval
import AS.Types.Formats
import AS.Types.Messages
import AS.Types.Network
import AS.Types.Shift
import AS.Types.Excel hiding (dbConn)
import AS.Types.User hiding (userId)
import AS.Types.Commits

import AS.Kernels.Python.Client

import AS.Eval.Core
import AS.Parsing.Substitutions
import AS.Parsing.References (getDependencies) 

import qualified Data.Map as M
import Database.Redis (Connection)
import Control.Monad (forM_, forever, (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (left)
import Data.Maybe
import AS.Logging

import AS.DB.Users (getOpenedSheets)


-- | Conditionally formats the cells based on the set of rules passed in -- all conditional formatting originally
-- in the cells go away. 
-- 
-- Note: The cells passed into conditionallyFormatCell should contain the most recent values for the cells.
-- In particular, if this was called through an eval, the values of ASCell should
-- agree with the values in EvalContext. If not, they should be the most recent values in the DB.
-- timchu, 12/17/15.
conditionallyFormatCells :: MessageContext -> 
                            EvalContext -> 
                            [ASCell] -> 
                            [CondFormatRule] -> 
                            EvalChainFunc -> 
                            EitherTExec [ASCell]
conditionallyFormatCells msgctx evalctx cells rules f = do
  let cells' = map (cellProps %~ clearCondFormatProps) cells
      transforms = map (ruleToCellTransform msgctx evalctx f) rules
      transformsComposed = foldr (>=>) return transforms
  mapM transformsComposed cells'

-- #needsrefactor will eventually have to change ranges to refs in CondFormatRule
-- Requires that v is the most up to date ASValue at location l whenever this function is called.
ruleToCellTransform ::  MessageContext -> 
                        EvalContext -> 
                        EvalChainFunc -> 
                        CondFormatRule -> 
                        (ASCell -> EitherTExec ASCell)
ruleToCellTransform msgctx evalctx f (CondFormatRule _ rngs condMapConstructor) c = 
  let l = c^.cellLocation in 
  case find (flip rangeContainsIndex l) rngs of 
    Nothing -> return c
    Just rng -> do
      let tl = getTopLeft rng
          offset = getIndicesOffset tl l
          shiftXp = shiftExpression offset
          determineFormats v = case condMapConstructor of 
            BoolFormatMapConstructor boolCond props -> do 
              let mEvalLoc = shiftSafe offset tl
              case mEvalLoc of 
                Nothing -> return []
                Just evalLoc -> do 
                  let shiftAndEvalExpr = (evaluateBoolExpression msgctx evalctx evalLoc f) . shiftXp 
                  shouldFormat <- checkBoolCond boolCond v shiftAndEvalExpr
                  return $ if shouldFormat then props else []
            LambdaFormatMapConstructor lambdaExpr -> do 
              let shiftedLambdaExpr = shiftXp (Expression lambdaExpr Python)
              formatResult <- evaluateFormatExpression msgctx evalctx shiftedLambdaExpr v f
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
updatedContextForEval :: MessageContext -> EvalContext -> ASExpression -> EitherTExec EvalContext
updatedContextForEval msgctx evalctx xp = do
  let valMap = evalctx^.virtualCellsMap
      conn = msgctx^.dbConnection
      sid = messageSheetId msgctx
  sheets <- liftIO $ getOpenedSheets conn $ msgctx^.userClient.userId
  let deps = getDependencies sid sheets xp 
  depInds <- concat <$> mapM (refToIndicesInCondFormatting conn) deps
  let depIndsToGet = filter (not . (flip M.member) valMap) depInds
  cells <- lift $ DB.getPossiblyBlankCells conn depIndsToGet
  let valMap' = insertMultiple valMap depIndsToGet cells
  -- #RoomForImprovement. Can prob,ably use %~ and make this cleaner.
  return $ evalctx & virtualCellsMap .~ valMap'

evaluateBoolExpression :: MessageContext ->
                          EvalContext -> 
                          ASIndex -> 
                          EvalChainFunc -> 
                          ASExpression -> 
                          EitherTExec ASValue
evaluateBoolExpression msgctx evalctx evalLoc f xp@(Expression str lang) = do
  evalctx' <- updatedContextForEval msgctx evalctx xp
  (Formatted (EvalResult res _) _) <- evaluateLanguage msgctx evalctx' evalLoc xp f
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
evaluateFormatExpression :: MessageContext ->
                            EvalContext -> 
                            ASExpression -> 
                            ASValue -> 
                            EvalChainFunc -> 
                            EitherTExec FormatResult
evaluateFormatExpression msgctx evalctx lambdaExpr v f = do
  evalctx' <- updatedContextForEval msgctx evalctx lambdaExpr
  lambdaExpr' <- lift $ insertValues msgctx evalctx' lambdaExpr f
  evaluateLambdaFormat (messageWorkbookId msgctx) lambdaExpr' v
