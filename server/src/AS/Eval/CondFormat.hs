module AS.Eval.CondFormat where

import AS.DB.API as DB
import AS.DB.Eval

import AS.Types.Cell
import AS.Types.Messages
import AS.Types.Eval
import AS.Types.Errors
import AS.Types.CondFormat


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


-- | Conditionally formats the cells based on the set of rules passed in -- all conditional formatting originally
-- in the cells go away. 
-- 
-- Note: The cells passed into conditionallyFormatCell should contain the most recent values for the cells.
-- In particular, if this was called through an eval, the values of ASCell should
-- agree with the values in EvalContext. If not, they should be the most recent values in the DB.
-- timchu, 12/17/15.
conditionallyFormatCells :: Connection -> ASSheetId -> [ASCell] -> [CondFormatRule] -> EvalContext -> EitherTExec [ASCell]
conditionallyFormatCells conn origSid cells rules ctx = do
  let cells' = map (\c -> c { cellProps = clearCondFormatProps (cellProps c) }) cells
      transforms = map (ruleToCellTransform conn origSid ctx) rules
      transformsComposed = foldr (>=>) return transforms
  mapM transformsComposed cells'

-- #needsrefactor will eventually have to change ranges to refs in CondFormatRule
-- Requires that v is the most up to date ASValue at location l whenever this function is called.
ruleToCellTransform :: Connection -> ASSheetId -> EvalContext -> CondFormatRule -> (ASCell -> EitherTExec ASCell)
ruleToCellTransform conn sid ctx cfr@(CondFormatRule _ rngs condFormatCondition format) c@(Cell l e v ps) = do
  let containingRange = find (flip rangeContainsIndex l) rngs
  case containingRange of
    Nothing -> return c
    Just rng -> do
      let tl = getTopLeft rng
          eval = evaluateExpression conn sid ctx
          offset = getIndicesOffset tl l
          shiftXp = shiftExpression offset
          shiftAndEvaluateExpression = eval . shiftXp
      mc <- checker condFormatCondition v shiftAndEvaluateExpression
      if mc
         then return $ Cell l e v (setCondFormatProp format ps)
         else return c

evaluateExpression :: Connection -> ASSheetId -> EvalContext -> ASExpression -> EitherTExec ASValue
evaluateExpression conn sid ctx xp@(Expression str lang) = do
  let dummyLoc = Index sid (-1,-1) -- #needsrefactor sucks. evaluateLanguage should take in a Maybe index. Until then
      valMap = virtualCellsMap ctx
      deps = getDependencies sid xp -- #needsrefactor will compress these all to indices
  depInds <- concat <$> mapM (refToIndices conn) deps
  let depIndsToGet = filter (not . (flip M.member) valMap) depInds
  cells <- lift $ DB.getPossiblyBlankCells conn depIndsToGet
  let valMap' = insertMultiple valMap depIndsToGet cells
      ctx' = ctx { virtualCellsMap = valMap' }
  (Formatted res _) <- evaluateLanguage conn dummyLoc ctx' xp
  case res of
    Expanding expandingValue -> left $ CondFormattingError ("Tried to apply conditional formatting rule" ++ str ++ "but got ExpandingValue error with expandingValue:  " ++ show expandingValue)
    CellValue (ValueError msg _) -> do
      let errMsg = "Tried to apply conditional formatting rule " ++ str ++ " but got error" ++ (show msg) ++ ". "
      left $ CondFormattingError errMsg
    CellValue v -> return v
