module AS.Handlers.Paste (handleCopy, handleCut, performCopy) where

import AS.Prelude
import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User (UserID)
import AS.Types.Excel hiding (dbConn)
import AS.Types.Eval
import AS.Types.Commits
import AS.Types.Shift

import AS.Parsing.Substitutions
import AS.Parsing.Excel
import AS.Parsing.References (exRefToASRef)

import AS.DB.Internal
import AS.DB.API
import AS.DB.Graph

import AS.Util
import AS.Dispatch.Core
import AS.Reply

import Control.Concurrent
import qualified Control.Lens as Lens

import Database.Redis (Connection)
import Control.Monad ((>=>))

handleCopy :: MessageContext -> ASRange -> ASRange -> IO ()
handleCopy msgctx from to = do
  toCells <- getCopyCells (msgctx^.dbConnection) from to
  errOrUpdate <- runDispatchCycle msgctx toCells DescendantsWithParent id
  broadcastErrOrUpdate msgctx errOrUpdate

handleCut :: MessageContext -> ASRange -> ASRange -> IO ()
handleCut msgctx from to = do
  newCells <- getCutCells (msgctx^.dbConnection) from to
  errOrUpdate <- runDispatchCycle msgctx newCells DescendantsWithParent id
  broadcastErrOrUpdate msgctx errOrUpdate

-- #needsrefactor currently exists for testing purposes only; doesn't require user connection. 
-- could restructure this in such a way that encapsulation is not broken. 
performCopy :: MessageContext -> ASRange -> ASRange -> IO (Either ASExecError SheetUpdate)
performCopy msgctx from to = do 
  toCells <- getCopyCells (msgctx^.dbConnection) from to
  runDispatchCycle msgctx toCells DescendantsWithParent id

----------------------------------------------------------------------------------------------------------------------------------------------
-- Copy helpers
----------------------------------------------------------------------------------------------------------------------------------------------

getCopyOffSets :: ASRange -> ASRange -> [Offset]
getCopyOffSets from to = offsets
  where
    fromDims = getFiniteRangeDims from
    toDims = getFiniteRangeDims to
    xRep = max 1 ((width toDims)^.int `div` (width fromDims)^.int)
    yRep = max 1 ((height toDims)^.int `div` (height fromDims)^.int)
    xRepOffsets = take xRep [0, (width fromDims)^.int..]
    yRepOffsets = take yRep [0, (height fromDims)^.int..]
    tlOffset = getRangeOffset from to
    offsets = [Offset { dCol = (dCol tlOffset) + Col x, dRow = (dRow tlOffset) + Row y } | x <- xRepOffsets, y <- yRepOffsets]

-- | Gets you the new cells to eval after shifting from a copy/paste. 
getCopyCells :: Connection -> ASRange -> ASRange -> IO [ASCell]
getCopyCells conn from to = do 
  fromCells          <- getPossiblyBlankCells conn (finiteRangeToIndices from)
  sanitizedFromCells <- sanitizeCopyCells conn fromCells from
  let offsets       = getCopyOffSets from to  -- how much to shift these cells for copy/copy/paste
      -- technically, this function should be shiftCell. shiftCell should _always_ shift the inner range key as well.
      translateCell :: Offset -> ASCell -> Maybe ASCell
      translateCell o c = shiftRangeKey o =<< shiftCell o c -- remember to shift the range key of a coupled expression as well
      toCells       = catMaybes $ concatMap (\o -> map (translateCell o) sanitizedFromCells) offsets
      updateSheetId = Lens.set locSheetId (rangeSheetId to)
      toCells'      = map (cellLocation %~ updateSheetId) toCells
  return toCells'

-- Same as sanitizeCutCells, except if everything is a list head, leave it as is. 
sanitizeCopyCells :: Connection -> [ASCell] -> ASRange -> IO [ASCell]
sanitizeCopyCells conn cells from
  | all isFatCellHead cells = return $ map toUncoupled cells 
  | otherwise = sanitizeCutCells conn cells from 


----------------------------------------------------------------------------------------------------------------------------------------------
-- Cut helpers
----------------------------------------------------------------------------------------------------------------------------------------------

-- | Offsets all the references in an expression that are contained in the cut range (passed in as 
-- the argument "from"). 
shiftExpressionForCut :: ASRange -> Offset -> ASExpression -> ASExpression
shiftExpressionForCut from offset xp = xp'
  where 
    fromSid     = rangeSheetId from
    shouldShift = (rangeContainsRef from) . (exRefToASRef (rangeSheetId from) []) -- don't need to consider sheets here, because this is a simple rectangle-overlap check.
    shiftFunc   = \ref -> if (shouldShift ref) then (shiftExRefForced offset ref) else ref
    xp'         = replaceRefs (show . shiftFunc) xp

shiftRangeKey :: Offset -> ASCell -> Maybe ASCell
shiftRangeKey offset c = case c^.cellRangeKey of 
  Nothing -> Just c
  Just (RangeKey ind dims) -> 
    ((c &) . Lens.set cellRangeKey . Just . flip RangeKey dims) <$> shiftSafe offset ind
    
getCutCells :: Connection -> ASRange -> ASRange -> IO [ASCell]
getCutCells conn from to = do 
  let offset = getRangeOffset from to
  toCells      <- getCutToCells conn from offset
  newDescCells <- getCutNewDescCells conn from offset
  let blankedCells = blankCellsAt (finiteRangeToIndices from) -- want to forget about tags
  -- precedence: toCells > updated descendant cells > blank cells
  return $ mergeCells toCells (mergeCells newDescCells blankedCells)

-- #expert
replaceCellLocsMaybe :: (ASIndex -> Maybe ASIndex) -> ASCell -> Maybe ASCell
replaceCellLocsMaybe f c = (flip (Lens.set cellLocation) c) <$> (f $ c^.cellLocation)

-- | Constructs the cells at the locations you'll be pasting to
getCutToCells :: Connection -> ASRange -> Offset -> IO [ASCell]
getCutToCells conn from offset = do 
  fromCells          <- getPossiblyBlankCells conn (finiteRangeToIndices from)
  sanitizedFromCells <- sanitizeCutCells conn fromCells from
  let shiftLoc    = shiftSafe offset
      changeExpr  = shiftExpressionForCut from offset
      modifyCell  = (shiftRangeKey offset) >=> (replaceCellLocsMaybe shiftLoc) . (cellExpression %~ changeExpr)
  return $ catMaybes $ map modifyCell sanitizedFromCells

-- | Returns the cells that reference the cut cells with their expressions updated. 
getCutNewDescCells :: Connection -> ASRange -> Offset -> IO [ASCell]
getCutNewDescCells conn from offset = do 
  immDescLocs <- getImmediateDescendantsForced (finiteRangeToIndices from)
  let immDescLocs' = filter (not . (rangeContainsIndex from)) immDescLocs
      changeExpr   = shiftExpressionForCut from offset
  descs <- catMaybes <$> getCells conn immDescLocs'
  return $ map (cellExpression %~ changeExpr) descs

-- | Decouples cells appropriately for re-eval on cut/paste, as follows:
--   * if a cell is not a part of a list, leave it as is. 
--   * if an entire list is contained in the range, keep just the head of the list. (So on eval
--     the entire list is re-evaluated)
--   * if a cell is part of a list that is not contained entirely in the selection, decouple it. 
sanitizeCutCells :: Connection -> [ASCell] -> ASRange -> IO [ASCell]
sanitizeCutCells conn cells from = do 
  keys <- fatCellsInRange conn from
  let (fatCellMembers, regularCells)  = partition isCoupled cells
      (containedCells, cutoffCells)   = partitionByRangeKey fatCellMembers keys
      decoupledCells                  = map toDecoupled cutoffCells
  return $ regularCells ++ decoupledCells ++ containedCells
