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

import AS.Logging


-- The cells passed into conditionallyFormatCell should be the most recently passed in value.
-- In particular, if this was called through an eval, the values of ASCell should
-- agree with the values in EvalContext. If not, they should be the most recent values anyways.
-- timchu, 12/17/15 (with help from Alex).
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
      -- TODO: timchu, 12/16/15. Can definitely split this into multiple functions.
      meetsCondition <- case cfc of
           CustomExpressionCondition cond -> do
             let cond' = shiftExpression offset cond
             xpVal <- evalXp conn sid ctx cond'
             if xpVal == ValueB True
                then return True
                else return False
           NoExpressionsCondition eType ->
             return $ (functionFromNoExpressionsType eType) v
           OneExpressionCondition eType cond -> do
             let cond' = shiftExpression offset cond
             xpVal <- evalXp conn sid ctx cond'
             -- TODO: Timchu, haven't thoroughly checked if this is right.
             return $ (functionFromOneExpressionType eType) v xpVal
           TwoExpressionsCondition eType condOne condTwo -> do
             let condOne' = shiftExpression offset condOne
             let condTwo' = shiftExpression offset condTwo
             xpValOne <- evalXp conn sid ctx condOne'
             xpValTwo <- evalXp conn sid ctx condTwo'
             return $ (functionFromTwoExpressionsType eType) v xpValOne xpValTwo
      if meetsCondition
         then return $ Cell l e v (setCondFormatProp format ps)
         else return c

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
  case res of
    Expanding expandingValue -> left $ CondFormattingError ("Tried to apply conditional formatting rule" ++ str ++ "but got ExpandingValue error with expandingValue:  " ++ show expandingValue)
    CellValue (ValueError msg _) -> do
      let errMsg = "Tried to apply conditional formatting rule " ++ str ++ " but got error" ++ (show msg) ++ ". "
      left $ CondFormattingError errMsg
    CellValue v -> return v
