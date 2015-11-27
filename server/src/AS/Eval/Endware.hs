module AS.Eval.Endware where

import Prelude
import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages
import AS.Types.Commits

import AS.Util as U
import AS.Eval.Core
import AS.DB.API as DB
import AS.Parsing.Substitutions
import Data.List

import Data.Char (isPunctuation, isSpace, toUpper)
import Data.Monoid (mappend)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Map as M

import Control.Exception
import Control.Monad (forM_, forever, (>=>))
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (left)
import Data.Aeson hiding (Success)
import qualified Network.WebSockets as WS

import Database.Redis as R
import AS.Daemon as DM


-- | Here, we apply a stack of endwares.
-- | Endware for producing tags post-eval e.g. streaming or styling
-- | Examples: green(x) in python -> produces styled value with string in output -> string parsed to Color tag
-- | Bloomberg(x) in java -> produces json with stream specs -> converted to Stream tag, kickoff daemon

evalEndware :: MVar ServerState -> [ASCell] -> CommitSource -> [ASCell] -> FormattedValMap -> EitherTExec [ASCell]
evalEndware state finalCells (sid, uid) origCells valMap = do 
  mapM_ (\c -> lift $ DM.possiblyCreateDaemon state uid c) origCells
  conn <- lift $ dbConn <$> readMVar state
  let cells1 = changeExcelExpressions finalCells
  cells2 <- conditionallyFormatCells conn sid cells1 valMap
  printDebugT "cells1" cells1
  printDebugT "cells2" cells2
  return cells2
   
----------------------------------------------------------------------------------------------------------------------------------------------
-- Endwares

tagStyledCells :: [ASCell] -> [ASCell]
tagStyledCells = id

changeExcelExpressions :: [ASCell] -> [ASCell]
changeExcelExpressions = id
-- L.map upperCase
-- 	where
-- 		upperCase :: ASCell -> ASCell
-- 		upperCase (Cell l (Expression e Excel) v t) = Cell l (Expression e' Excel) v t 
-- 			where 
-- 				e' = L.map toUpper e
-- 		upperCase c = c
-- #incomplete should change all function names to upper-cased forms

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
  printDebugT "res" res
  case res of
    CellValue (ValueB b) -> return b
    val                  -> do 
      let errMsg = "Tried to apply " ++ str ++ " in " ++ (show lang) ++ " but got non-boolean value " ++ (show val) ++ ". "
      left $ CondFormattingError errMsg