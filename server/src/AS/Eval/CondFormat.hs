module AS.Eval.CondFormat (conditionallyFormatCells) where

import AS.DB.API as DB
import AS.Util as U

import AS.Types.Cell
import AS.Types.Messages

import AS.Eval.Core
import AS.Parsing.Substitutions

import Prelude
import Data.List
import qualified Data.Map as M
import Database.Redis as R
import Control.Monad (forM_, forever, (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (left)
import Data.Maybe

conditionallyFormatCells :: R.Connection -> ASSheetId -> [ASCell] -> FormattedValMap -> EitherTExec [ASCell]
conditionallyFormatCells conn origSid cells valMap = do 
  rules <- lift $ DB.getCondFormattingRules conn origSid
  let transforms = map (ruleToCellTransform origSid valMap) rules
      transformsComposed = foldr (>=>) return transforms
  mapM transformsComposed cells

-- #needsrefactor will eventually have to change ranges to refs in CondFormatRule
ruleToCellTransform :: ASSheetId -> FormattedValMap -> CondFormatRule -> (ASCell -> EitherTExec ASCell)
ruleToCellTransform sid valMap (CondFormatRule rngs cond format) c@(Cell l e v ps) = do
  let containingRange = find (flip U.rangeContainsIndex l) rngs
  case containingRange of 
    Nothing -> return c
    Just rng -> do
      let tl = U.getTopLeft rng
          offset = U.getIndicesOffset tl l
          cond'  = shiftExpression offset cond
      satisfiesCond <- meetsCondition sid valMap cond' v
      if satisfiesCond
        then return $ Cell l e v (setProp format ps)
        else return c

meetsCondition :: ASSheetId -> FormattedValMap -> ASExpression -> ASValue -> EitherTExec Bool
meetsCondition sid valMap xp@(Expression str lang) v = do 
  let dummyLoc = Index sid (-1,-1) -- #needsrefactor sucks. evaluateLanguage should take in a Maybe index. Until then 
      deps = getDependencies sid xp -- #needsrefactor will compress these all to indices
      depSets = map refToIndices deps   -- :: [Maybe [ASIndex]]
      depInds = concat $ catMaybes depSets
      depIndsToGet = filter (not . (flip M.member) valMap) depInds
  cells <- lift $ DB.getPossiblyBlankCells depIndsToGet
  let getFormat = \ps -> formatType <$> getProp ValueFormatProp ps
      valMapToMerge = map (\(Cell l _ v ps) -> (l, Formatted (CellValue v) $ getFormat ps)) cells 
      valMap' = M.union valMap (M.fromList valMapToMerge)
  (Formatted res _) <- evaluateLanguage sid dummyLoc valMap' xp
  case res of
    CellValue (ValueB b) -> return b
    val                  -> do 
      let errMsg = "Tried to apply " ++ str ++ " in " ++ (show lang) ++ " but got non-boolean value " ++ (show val) ++ ". "
      left $ CondFormattingError errMsg