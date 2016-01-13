module AS.Eval.CondFormat where

import Prelude()
import AS.Prelude

import AS.DB.API as DB
import AS.DB.Eval

import AS.Types.Cell
import AS.Types.Messages
import AS.Types.Eval
import AS.Types.Errors
import AS.Types.CondFormat
import AS.Types.Network

import AS.Eval.Core
import AS.Parsing.Substitutions
import AS.Util (insertMultiple)

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
conditionallyFormatCells :: ServerState -> ASSheetId -> [ASCell] -> [CondFormatRule] -> EvalContext -> EitherTExec [ASCell]
conditionallyFormatCells state origSid cells rules ctx = do
  let cells' = map (cellProps %~ clearCondFormatProps) cells
      transforms = map (ruleToCellTransform state origSid ctx) rules
      transformsComposed = foldr (>=>) return transforms
  mapM transformsComposed cells'

-- #needsrefactor will eventually have to change ranges to refs in CondFormatRule
-- Requires that v is the most up to date ASValue at location l whenever this function is called.
ruleToCellTransform :: ServerState -> ASSheetId -> EvalContext -> CondFormatRule -> (ASCell -> EitherTExec ASCell)
ruleToCellTransform state sid ctx (CondFormatRule _ rngs condFormatCondition) c = 
  let l = c^.cellLocation in 
  case find (flip rangeContainsIndex l) rngs of 
    Nothing -> return c
    Just rng -> do
      let tl = getTopLeft rng
          eval = evaluateExpression state sid ctx
          offset = getIndicesOffset tl l
          shiftXp = shiftExpression offset
          shiftAndEvaluateExpression = eval . shiftXp
          determineFormat v = case condFormatCondition of 
            BoolCondition boolCond prop -> do 
              let shiftAndEvalExpr = (evaluateExpression state sid ctx) . shiftXp 
              shouldFormat <- checkBoolCond boolCond v shiftAndEvalExpr
              return $ if shouldFormat then Just prop else Nothing
      conditionalFormat <- determineFormat $ c^.cellValue
      return $ case conditionalFormat of
        Nothing -> c
        Just format -> c & cellProps %~ setCondFormatProp format

evaluateExpression :: ServerState -> ASSheetId -> EvalContext -> ASExpression -> EitherTExec ASValue
evaluateExpression state sid ctx xp@(Expression str lang) = do
  let dummyLoc = Index sid (Coord (-1) (-1)) -- #needsrefactor sucks. evaluateLanguage should take in a Maybe index. Until then
      valMap = virtualCellsMap ctx
      deps = getDependencies sid xp -- #needsrefactor will compress these all to indices
      conn = state^.dbConn
  depInds <- concat <$> mapM (refToIndices conn) deps
  let depIndsToGet = filter (not . (flip M.member) valMap) depInds
  cells <- lift $ DB.getPossiblyBlankCells conn depIndsToGet
  let valMap' = insertMultiple valMap depIndsToGet cells
      ctx' = ctx { virtualCellsMap = valMap' }
  -- we don't care about the display string produced by evaluateLanguage
  (Formatted (EvalResult res _) _) <- evaluateLanguage state dummyLoc ctx' xp
  case res of
    Expanding expandingValue -> left $ CondFormattingError ("Tried to apply conditional formatting rule" ++ str ++ "but got ExpandingValue error with expandingValue:  " ++ show expandingValue)
    CellValue (ValueError msg _) -> do
      let errMsg = "Tried to apply conditional formatting rule " ++ str ++ " but got error" ++ (show msg) ++ ". "
      left $ CondFormattingError errMsg
    CellValue v -> return v
