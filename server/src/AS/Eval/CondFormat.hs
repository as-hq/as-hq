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
ruleToCellTransform conn sid ctx (CondFormatRule rngs cond format) c@(Cell l e v ps) = do
  let containingRange = find (flip rangeContainsIndex l) rngs
  case containingRange of
    Nothing -> return c
    Just rng -> do
      let tl = getTopLeft rng
          offset = getIndicesOffset tl l
          cond'  = shiftExpression offset cond
      satisfiesCond <- meetsCondition conn sid ctx cond' v
      if satisfiesCond
        then return $ Cell l e v (setCondFormatProp format ps)
        else return c

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
    CellValue (ValueB b) -> return b
    val                  -> do
      let errMsg = "Tried to apply " ++ str ++ " in " ++ (show lang) ++ " but got non-boolean value " ++ (show val) ++ ". "
      left $ CondFormattingError errMsg
