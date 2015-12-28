module AS.Handlers.Paste (handleCopy, handleCut) where

import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User
import AS.Types.Excel hiding (dbConn)
import AS.Types.Eval

import AS.Parsing.Substitutions
import AS.Parsing.Excel

import AS.DB.Internal
import AS.DB.API
import AS.DB.Graph

import AS.Util
import AS.Dispatch.Core
import AS.Reply

import Data.Maybe
import Data.List
import Control.Concurrent

import Database.Redis (Connection)
import Control.Monad ((>=>))

handleCopy :: ASUserClient -> MVar ServerState -> ASRange -> ASRange -> IO ()
handleCopy uc state from to = do
  putStrLn $ "IN HANDLE COPY"
  conn <- dbConn <$> readMVar state
  toCells <- getCopyCells conn from to
  errOrCommit <- runDispatchCycle state toCells DescendantsWithParent (userCommitSource uc) id
  broadcastFiltered state uc $ makeReplyMessageFromErrOrCommit errOrCommit

handleCut :: ASUserClient -> MVar ServerState -> ASRange -> ASRange -> IO ()
handleCut uc state from to = do
  conn <- dbConn <$> readMVar state
  newCells <- getCutCells conn from to
  errOrCommit <- runDispatchCycle state newCells DescendantsWithParent (userCommitSource uc) id
  broadcastFiltered state uc $ makeReplyMessageFromErrOrCommit errOrCommit


----------------------------------------------------------------------------------------------------------------------------------------------
-- Copy helpers
----------------------------------------------------------------------------------------------------------------------------------------------

getCopyOffSets :: ASRange -> ASRange -> [Offset]
getCopyOffSets from to = offsets
  where
    fromDims = getRangeDims from
    toDims = getRangeDims to
    xRep = max 1 ((width toDims) `div` (width fromDims))
    yRep = max 1 ((height toDims) `div` (height fromDims))
    xRepOffsets = take xRep [0, (width fromDims)..]
    yRepOffsets = take yRep [0, (height fromDims)..]
    tlOffset = getRangeOffset from to
    offsets = [Offset { dX = (dX tlOffset) + x, dY = (dY tlOffset) + y } | x <- xRepOffsets, y <- yRepOffsets]

-- | Gets you the new cells to eval after shifting from a copy/paste. 
getCopyCells :: Connection -> ASRange -> ASRange -> IO [ASCell]
getCopyCells conn from to = do 
  fromCells          <- getPossiblyBlankCells conn (rangeToIndices from)
  sanitizedFromCells <- sanitizeCopyCells conn fromCells from
  let offsets       = getCopyOffSets from to  -- how much to shift these cells for copy/copy/paste
      -- technically, this function should be shiftCell. shiftCell should _always_ shift the inner range key as well.
      translateCell :: Offset -> ASCell -> Maybe ASCell
      translateCell o c = shiftRangeKey o =<< shiftCell o c -- remember to shift the range key of a coupled expression as well
      toCells       = catMaybes $ concatMap (\o -> map (translateCell o) sanitizedFromCells) offsets
      updateSheetId = \l -> l { locSheetId = rangeSheetId to }
      toCells'      = map (replaceCellLocs updateSheetId) toCells
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
    shouldShift = (rangeContainsRef from) . (exRefToASRef fromSid)
    shiftFunc   = \ref -> if (shouldShift ref) then (shiftExRefForced offset ref) else ref
    xp'         = replaceRefs (show . shiftFunc) xp

replaceCellLocs :: (ASIndex -> ASIndex) -> ASCell -> ASCell
replaceCellLocs f c = c { cellLocation = f $ cellLocation c }

replaceCellLocsMaybe :: (ASIndex -> Maybe ASIndex) -> ASCell -> Maybe ASCell
replaceCellLocsMaybe f c = case f $ cellLocation c of 
  Nothing -> Nothing 
  Just l -> Just $ c { cellLocation = l }

replaceCellExpressions :: (ASExpression -> ASExpression) -> ASCell -> ASCell
replaceCellExpressions f c = c { cellExpression = f $ cellExpression c }

shiftRangeKey :: Offset -> ASCell -> Maybe ASCell
shiftRangeKey offset c@(Cell _ (Expression _ _) _ _) = Just c
shiftRangeKey offset (Cell l (Coupled xp lang typ (RangeKey ind dims)) v ts) = case ind' of 
  Nothing -> Nothing
  Just i  -> Just $ Cell l (Coupled xp lang typ (RangeKey i dims)) v ts
  where ind' = shiftInd offset ind

getCutCells :: Connection -> ASRange -> ASRange -> IO [ASCell]
getCutCells conn from to = do 
  let offset = getRangeOffset from to
  toCells      <- getCutToCells conn from offset
  newDescCells <- getCutNewDescCells conn from offset
  let blankedCells = blankCellsAt (rangeToIndices from) -- want to forget about tags
  -- precedence: toCells > updated descendant cells > blank cells
  return $ mergeCells toCells (mergeCells newDescCells blankedCells)

-- | Constructs the cells at the locations you'll be pasting to
getCutToCells :: Connection -> ASRange -> Offset -> IO [ASCell]
getCutToCells conn from offset = do 
  fromCells          <- getPossiblyBlankCells conn (rangeToIndices from)
  sanitizedFromCells <- sanitizeCutCells conn fromCells from
  let shiftLoc    = shiftInd offset
      changeExpr  = shiftExpressionForCut from offset
      modifyCell  = (shiftRangeKey offset) >=> (replaceCellLocsMaybe shiftLoc) . (replaceCellExpressions changeExpr)
  return $ catMaybes $ map modifyCell sanitizedFromCells

-- | Returns the cells that reference the cut cells with their expressions updated. 
getCutNewDescCells :: Connection -> ASRange -> Offset -> IO [ASCell]
getCutNewDescCells conn from offset = do 
  immDescLocs <- getImmediateDescendantsForced (rangeToIndices from)
  let immDescLocs' = filter (not . (rangeContainsIndex from)) immDescLocs
      changeExpr   = shiftExpressionForCut from offset
  descs <- catMaybes <$> getCells conn immDescLocs'
  return $ map (replaceCellExpressions changeExpr) descs

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