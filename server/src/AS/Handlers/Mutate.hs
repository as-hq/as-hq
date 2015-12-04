module AS.Handlers.Mutate (handleMutateSheet) where

import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User
import AS.Types.Excel
import AS.Types.Eval

import AS.DB.Internal as DI
import qualified Data.Text as T

import AS.Parsing.Substitutions
import AS.Reply
import AS.Util as U
import AS.DB.API as DB
import AS.Dispatch.Core

import Control.Concurrent
import Data.Maybe

handleMutateSheet :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleMutateSheet uc state (PayloadMutate mutateType) = do 
  conn <- dbConn <$> readMVar state
  allCells <- DB.getCellsInSheet conn (userSheetId uc)
  let newCells = map (cellMap mutateType) allCells
      oldCellsNewCells = zip allCells newCells
      oldCellsNewCells' = filter (\(c, c') -> (isNothing c') || (c /= fromJust c')) oldCellsNewCells
      -- ^ get rid of cells that haven't changed.
      oldCells' = map fst oldCellsNewCells
      blankedCells = blankCellsAt (map cellLocation oldCells')
      newCells' = catMaybes $ map snd oldCellsNewCells
      updatedCells   = mergeCells newCells' blankedCells -- eval blanks at the old cell locations, re-eval at new locs
  updateMsg <- runDispatchCycle state updatedCells DescendantsWithParent (userCommitSource uc)
  broadcastFiltered state uc updateMsg


cellLocMap :: MutateType -> (ASIndex -> Maybe ASIndex)
cellLocMap (InsertCol c') (Index sid (c, r)) = Just $ Index sid (if c >= c' then c+1 else c, r)
cellLocMap (InsertRow r') (Index sid (c, r)) = Just $ Index sid (c, if r >= r' then r+1 else r)
cellLocMap (DeleteCol c') i@(Index sid (c, r))
  | c == c'  = Nothing
  | c > c'   = Just $ Index sid (c-1, r)
  | c < c'   = Just i
cellLocMap (DeleteRow r') i@(Index sid (c, r))
  | r == r'  = Nothing
  | r > r'   = Just $ Index sid (c, r-1)
  | r < r'   = Just i
cellLocMap (DragCol oldC newC) i@(Index sid (c, r)) -- DragCol 3 1 : (123) -> (312)
  | c < min oldC newC = Just i
  | c > max oldC newC = Just i
  | c == oldC         = Just $ Index sid (newC, r) 
  | oldC < newC       = Just $ Index sid (c-1, r) -- here on we assume c is strictly between oldC and newC
  | oldC > newC       = Just $ Index sid (c+1, r)
  -- case oldC == newC can't happen because oldC < c < newC since third pattern-match
cellLocMap (DragRow oldR newR) i@(Index sid (c, r))
  | r < min oldR newR = Just i
  | r > max oldR newR = Just i
  | r == oldR         = Just $ Index sid (c, newR)
  | oldR < newR       = Just $ Index sid (c, r-1) -- here on we assume c is strictly between oldR and newR
  | oldR > newR       = Just $ Index sid (c, r+1)
  -- case oldR == newR can't happen because oldR < r < newR since third pattern-match

refMap :: MutateType -> (ExRef -> ExRef)
refMap mt ExOutOfBounds = ExOutOfBounds
refMap mt er@(ExLocRef (ExIndex rt _ _) ls lw) = er'
  where -- feels kinda ugly... 
    IndexRef ind = exRefToASRef (T.pack "") er
    er' = case (cellLocMap mt ind) of 
      Nothing -> ExOutOfBounds
      Just newRefLoc -> ExLocRef ei' ls lw
        where
          ExLocRef ei _ _ = asRefToExRef $ IndexRef newRefLoc
          ei' = ei { refType = rt }

refMap mt er@(ExRangeRef (ExRange f s) rs rw) = ExRangeRef (ExRange f' s') rs rw
  where 
    ExLocRef f' _ _ = refMap mt (ExLocRef f rs rw)
    ExLocRef s' _ _ = refMap mt (ExLocRef s rs rw)
refMap mt er@(ExPointerRef el ps pw) = ExPointerRef el' ps pw
  where 
    ExLocRef el' _ _ = refMap mt (ExLocRef el ps pw)

expressionMap :: MutateType -> (ASExpression -> ASExpression)
expressionMap mt = replaceRefs (show . (refMap mt))

cellMap :: MutateType -> (ASCell -> Maybe ASCell)
cellMap mt c@(Cell loc xp v ps) = case ((cellLocMap mt) loc) of 
  Nothing -> Nothing 
  Just loc' -> Just $ sanitizeMutateCell mt loc $ Cell loc' ((expressionMap mt) xp) v ps 

-- | If the cell passed in is uncoupled, leave it as is. Otherwise: 
-- * if its range would get decoupled by the mutation, decouple it. 
-- * if not, if it is a fat cell head, make it an uncoupled cell that'll get re-evaled. 
-- * if it is not a fat cell head, delete it. 
-- whether the fatcell it was a part of would get split up by the type of mutation.  
sanitizeMutateCell :: MutateType -> ASIndex -> ASCell -> ASCell
sanitizeMutateCell _ _ c@(Cell _ (Expression _ _) _ _) = c
sanitizeMutateCell mt oldLoc c = cell'
  where 
    Just rk = cellToRangeKey c
    cell' = if trace' "mutated?" $ fatCellGotMutated mt rk
      then DI.toDecoupled c
      else c { cellExpression = (cellExpression c) { cRangeKey = rk { keyIndex = fromJust $ cellLocMap mt (keyIndex rk) } } } 

between :: Int -> Int -> Int -> Bool
between lower upper x = (x >= lower) && (x <= upper)

fatCellGotMutated :: MutateType -> RangeKey -> Bool
fatCellGotMutated (InsertCol c) (RangeKey (Index _ (tlc, _)) (dC, _)) = between (tlc + 1) (tlc + dC - 1) c
fatCellGotMutated (InsertRow r) (RangeKey (Index _ (_, tlr)) (_, dR)) = between (tlr + 1) (tlr + dR - 1) r

fatCellGotMutated (DeleteCol c) (RangeKey (Index _ (tlc, _)) (dC, _)) = between tlc (tlc + dC - 1) c
fatCellGotMutated (DeleteRow r) (RangeKey (Index _ (_, tlr)) (_, dR)) = between tlr (tlr + dR - 1) r

fatCellGotMutated (DragCol _ _) (RangeKey _ (1, _)) = False
fatCellGotMutated (DragCol c1 c2) (RangeKey (Index _ (tlc, _)) (dC, _)) = or [
  between tlc (tlc + dC - 1) c1, 
  between (tlc + 1) (tlc + dC - 1) c2 ]

fatCellGotMutated (DragRow _ _) (RangeKey _ (_, 1)) = False
fatCellGotMutated (DragRow r1 r2) (RangeKey (Index _ (_, tlr)) (_, dR)) = or [ 
  between tlr (tlr + dR - 1) r1, 
  between (tlr + 1) (tlr + dR - 1) r2 ]