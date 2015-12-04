module AS.Handlers.Paste (handleCopy, handleCut) where

import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User
import AS.Types.Excel
import AS.Parsing.Substitutions

import AS.DB.Util
import AS.DB.API
import AS.DB.Graph

import AS.Parsing.Substitutions
import AS.Parsing.Excel
import AS.Util
import AS.Dispatch.Core
import AS.Reply

import Data.Maybe
import Data.List
import Control.Concurrent


import qualified Database.Redis as R

handleCopy :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleCopy uc state (PayloadPaste from to) = do
  conn <- dbConn <$> readMVar state
  toCells <- getCopyCells conn from to
  msg' <- runDispatchCycle state toCells False (userCommitSource uc)
  broadcastFiltered state uc msg'

handleCut :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleCut uc state (PayloadPaste from to) = do
  conn <- dbConn <$> readMVar state
  newCells <- getCutCells conn from to
  msg' <- runDispatchCycle state newCells False (userCommitSource uc)
  broadcastFiltered state uc msg'


----------------------------------------------------------------------------------------------------------------------------------------------
-- Copy helpers
----------------------------------------------------------------------------------------------------------------------------------------------

getCopyOffSets :: ASRange -> ASRange -> [Offset]
getCopyOffSets from to = offsets
  where
    (fromYDim, fromXDim) = getRangeDims from
    (toYDim, toXDim) = getRangeDims to
    yRep = max 1 (toYDim `div` fromYDim)
    xRep = max 1 (toXDim `div` fromXDim)
    (topYOffset, topXOffset) = getRangeOffset from to
    yRepOffsets = take yRep [0,fromYDim..]
    xRepOffsets = take xRep [0,fromXDim..]
    offsets = [(topYOffset + y, topXOffset + x) | y <- yRepOffsets, x <- xRepOffsets]

-- | Gets you the new cells to eval after shifting from a copy/paste. 
getCopyCells :: R.Connection -> ASRange -> ASRange -> IO [ASCell]
getCopyCells conn from to = do 
  fromCells          <- getPossiblyBlankCells (rangeToIndices from)
  sanitizedFromCells <- sanitizeCopyCells conn fromCells from
  let offsets       = getCopyOffSets from to  -- how much to shift these cells for copy/copy/paste
      toCells       = concat $ map (\o -> map (shiftCell o) sanitizedFromCells) offsets
      updateSheetId = \l -> l { locSheetId = rangeSheetId to }
      toCells'      = map (replaceCellLocs updateSheetId) toCells
  return toCells'

-- Same as sanitizeCutCells, except if everything is a list head, leave it as is. 
sanitizeCopyCells :: R.Connection -> [ASCell] -> ASRange -> IO [ASCell]
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

replaceCellExpressions :: (ASExpression -> ASExpression) -> ASCell -> ASCell
replaceCellExpressions f c = c { cellExpression = f $ cellExpression c }

getCutCells :: R.Connection -> ASRange -> ASRange -> IO [ASCell]
getCutCells conn from to = do 
  let offset = getRangeOffset from to
  toCells      <- getCutToCells conn from offset
  newDescCells <- getCutNewDescCells from offset
  let blankedCells = blankCellsAt (rangeToIndices from) -- want to forget about tags
  -- precedence: toCells > updated descendant cells > blank cells
  return $ mergeCells toCells (mergeCells newDescCells blankedCells)

-- | Constructs the cells at the locations you'll be pasting to
getCutToCells :: R.Connection -> ASRange -> Offset -> IO [ASCell]
getCutToCells conn from offset = do 
  fromCells          <- getPossiblyBlankCells (rangeToIndices from)
  sanitizedFromCells <- sanitizeCutCells conn fromCells from
  let shiftLoc    = shiftInd offset
      changeExpr  = shiftExpressionForCut from offset
  return $ map ((replaceCellLocs shiftLoc) . (replaceCellExpressions changeExpr)) sanitizedFromCells

-- | Returns the cells that reference the cut cells with their expressions updated. 
getCutNewDescCells :: ASRange -> Offset -> IO [ASCell]
getCutNewDescCells from offset = do 
  immDescLocs <- getImmediateDescendantsForced (rangeToIndices from)
  let immDescLocs' = filter (not . (rangeContainsIndex from)) immDescLocs
      changeExpr   = shiftExpressionForCut from offset
  descs <- catMaybes <$> getCells immDescLocs'
  return $ map (replaceCellExpressions changeExpr) descs

-- | Decouples cells appropriately for re-eval on cut/paste, as follows:
--   * if a cell is not a part of a list, leave it as is. 
--   * if an entire list is contained in the range, keep just the head of the list. (So on eval
--     the entire list is re-evaluated)
--   * if a cell is part of a list that is not contained entirely in the selection, decouple it. 
sanitizeCutCells :: R.Connection -> [ASCell] -> ASRange -> IO [ASCell]
sanitizeCutCells conn cells from = do 
  keys <- fatCellsInRange conn from
  let (fatCellMembers, regularCells)  = partition isFatCellMember cells
      (containedCells, cutoffCells)   = partitionByRangeKey fatCellMembers keys
      decoupledCells                  = map decoupleCell cutoffCells
      containedFatCellHeads           = filter isFatCellHead containedCells
      containedFatCellHeadsUncoupled  = map toUncoupled containedFatCellHeads
  return $ regularCells ++ decoupledCells ++ containedFatCellHeadsUncoupled