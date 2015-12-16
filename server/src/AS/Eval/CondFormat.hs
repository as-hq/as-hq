module AS.Eval.CondFormat (conditionallyFormatCells) where

import AS.DB.API as DB

import AS.Types.Cell
import AS.Types.Messages
import AS.Types.Eval
import AS.Types.Errors


import AS.Eval.Core
import AS.Parsing.Substitutions
import AS.Util (insertMultiple)

import Prelude
import Data.List
import qualified Data.Map as M
import Database.Redis (Connection)
import Control.Monad (forM_, forever, (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (left)
import Data.Maybe

conditionallyFormatCells :: Connection -> ASSheetId -> [ASCell] -> [CondFormatRule] -> EvalContext -> EitherTExec [ASCell]
conditionallyFormatCells conn origSid cells rules ctx = do
  let cells' = map (\c -> c { cellProps = clearCondFormatProps (cellProps c) }) cells
      transforms = map (ruleToCellTransform conn origSid ctx) rules
      transformsComposed = foldr (>=>) return transforms
  mapM transformsComposed cells'

-- #needsrefactor will eventually have to change ranges to refs in CondFormatRule
ruleToCellTransform :: Connection -> ASSheetId -> EvalContext -> CondFormatRule -> (ASCell -> EitherTExec ASCell)
ruleToCellTransform conn sid ctx cfr@(CondFormatRule rngs cfc format) c@(Cell l e v ps) = do
  let containingRange = find (flip rangeContainsIndex l) rngs
  case containingRange of
    Nothing -> return c
    Just rng -> do
      let tl = getTopLeft rng
          offset = getIndicesOffset tl l
      -- TODO: timchu, 12/16/15. Can definitely put this into one function.
      finalXpVal <- case cfc of
           OneExpressionCondition eType expression -> do
             let cond = oneExpression cfc
                 cond' = shiftExpression offset cond
             xpVal <- evalXp conn sid ctx cond'
             case eType of
                  CustomExpression -> return $ xpVal
                  InequalityExpression t -> do
                    let ineqSign = case t of
                                        GreaterThan -> ">"
                                        LessThan -> "<"
                                        Geq -> "<="
                                        Leq -> ">="
                                        Equals -> "=="
                                        NotEquals -> "!="
                        newCondXp = "=" ++ show tl ++ ineqSign ++ show xpVal
                    evalXp conn sid ctx (Expression newCondXp Excel)
           NoExpressionsCondition eType -> do
             case eType of
                  IsEmpty ->
                    evalXp conn sid ctx (Expression ("=ISBLANK(" ++ show tl ++ ")") Excel)
                  IsNotEmpty ->
                    evalXp conn sid ctx (Expression ("=NOT(ISBLANK(" ++ show tl ++ "))") Excel)
           TwoExpressionsCondition eType condOne condTwo -> do
             xpValOne <- evalXp conn sid ctx condOne
             xpValTwo <- evalXp conn sid ctx condTwo
             --TODO: timchu, this is a hack since I don't know which of xpValOne and xpValTwo are larger.
             let betweenXpOne = "AND(" ++ show xpValOne ++ ">=" ++ show tl ++ "," ++ show tl ++ ">=" ++ show xpValTwo ++ ")"
                 betweenXpTwo = "AND(" ++ show xpValTwo ++ ">=" ++ show tl ++ "," ++ show tl ++ ">=" ++ show xpValOne ++ ")"
                 betweenXp = "=OR(" ++ betweenXpOne ++ "," ++ betweenXpTwo ++ ")"
                 newCondXp = case eType of
                                  IsBetween ->
                                    betweenXp
                                  IsNotBetween ->
                                    "NOT(" ++ betweenXp ++ ")"
             evalXp conn sid ctx (Expression newCondXp Excel)
      if finalXpVal == ValueB True
         then return $ Cell l e v (setCondFormatProp format ps)
         else return c

-- get your value, shove it into a new ASExpresssion, finish.
-- TODO: Timchu, I have no idea what this code does!!!!
evalXp :: Connection -> ASSheetId -> EvalContext -> ASExpression -> EitherTExec ASValue
evalXp conn sid ctx xp@(Expression str lang) = do
  let dummyLoc = Index sid (-1,-1) -- #needsrefactor sucks. evaluateLanguage should take in a Maybe index. Until then
      valMap = contextMap ctx
      deps = getDependencies sid xp -- #needsrefactor will compress these all to indices
  depInds <- concat <$> mapM refToIndices deps
  let depIndsToGet = filter (not . (flip M.member) valMap) depInds
  cells <- lift $ DB.getPossiblyBlankCells depIndsToGet
  let valMap' = insertMultiple valMap depIndsToGet cells
      ctx' = ctx { contextMap = valMap' }
  (Formatted res _) <- evaluateLanguage conn dummyLoc ctx' xp
  -- TODO: timchu, 12/15/15. Not sure what to do on expanding values.
  case res of
    Expanding _ -> left $ CondFormattingError "An Expanding Value"
    CellValue (ValueError msg _) -> do
      let errMsg = "Tried to apply conditional formatting rule " ++ str ++ " but got error" ++ (show msg) ++ ". "
      left $ CondFormattingError errMsg
    CellValue v -> return v

-- EvalContext -> ASExpression -> ASValue
-- Can there be a comment on what the EvalContext, the ASEpxression, and the ASValue are here for?
meetsCondition :: Connection -> ASSheetId -> EvalContext -> ASExpression -> ASValue -> EitherTExec Bool
meetsCondition conn sid ctx xp@(Expression str lang) v = do
  let dummyLoc = Index sid (-1,-1) -- #needsrefactor sucks. evaluateLanguage should take in a Maybe index. Until then
      valMap = contextMap ctx
      deps = getDependencies sid xp -- #needsrefactor will compress these all to indices
  depInds <- concat <$> mapM refToIndices deps
  let depIndsToGet = filter (not . (flip M.member) valMap) depInds
  cells <- lift $ DB.getPossiblyBlankCells depIndsToGet
  let valMap' = insertMultiple valMap depIndsToGet cells
      ctx' = ctx { contextMap = valMap' }
  (Formatted res _) <- evaluateLanguage conn dummyLoc ctx' xp
  case res of
    CellValue (ValueB True)      -> return True
    CellValue (ValueError msg _) -> do
      let errMsg = "Tried to apply conditional formatting rule " ++ str ++ " but got error" ++ (show msg) ++ ". "
      left $ CondFormattingError errMsg
    otherwise                    -> return False
