module AS.Handlers.Mutate (handleMutateSheet) where

import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User
import AS.Types.Excel hiding (dbConn)
import AS.Types.Eval
import AS.Types.Commits
import AS.Types.Updates

import AS.DB.Internal as DI
import AS.Types.Bar
import qualified Data.Text as T

import AS.Parsing.Substitutions
import AS.Reply
import AS.Util as U
import AS.DB.API as DB
import AS.Dispatch.Core
import AS.Logging

import Control.Concurrent
import Data.Maybe

handleMutateSheet :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleMutateSheet uc state (PayloadMutate mutateType) = do
  let sid = userSheetId uc
  conn <- dbConn <$> readMVar state
  -- update cells 
  oldCells <- DB.getCellsInSheet conn (userSheetId uc)
  let newCells = map (cellMap mutateType) oldCells
      (oldCells', newCells') = keepUnequal $ zip oldCells newCells
      blankedCells = blankCellsAt $ map cellLocation oldCells'
      updatedCells = mergeCells newCells' blankedCells -- eval blanks at the old cell locations, re-eval at new locs
  
  -- update barProps
  oldBars <- DB.getBarsInSheet conn sid
  let newBars = map (barMap mutateType) oldBars
      (oldBars', newBars') = keepUnequal $ zip oldBars newBars
  -- propagate changes
  let bu = Update { oldKeys = map barIndex oldBars', newVals = newBars' }
      commitTransform = \update -> update { barUpdates = bu }
  errOrCommit <- runDispatchCycle state updatedCells DescendantsWithParent (userCommitSource uc) commitTransform
  broadcastFiltered state uc $ makeReplyMessageFromErrOrCommit errOrCommit

keepUnequal :: (Eq a) => [(a, Maybe a)] -> ([a], [a])
keepUnequal x = (ls1, ls2) 
  where 
    unequals = filter (\(c, c') -> (Just c /= c')) x
    ls1 = map      fst unequals
    ls2 = mapMaybe snd unequals

-- | For a mutate, maps the old row and column to the new row and column.
barIndexMap :: MutateType -> BarIndex -> Maybe BarIndex
barIndexMap (InsertCol c') bar@(BarIndex sid typ bari) =
  case typ of
       ColumnType -> Just $ BarIndex sid typ (if bari >= c' then bari+1 else bari)
       RowType -> Just bar
barIndexMap (InsertRow r') bar@(BarIndex sid typ bari) =
  case typ of
       ColumnType -> Just bar
       RowType -> Just $ BarIndex sid typ (if bari >= r' then bari+1 else bari)
barIndexMap (DeleteCol c') bar@(BarIndex sid typ bari) =
  case typ of
       ColumnType | bari == c' -> Nothing
                     | otherwise -> Just $ BarIndex sid typ (if bari >= c' then bari-1 else bari)
       RowType -> Just bar
barIndexMap (DeleteRow r') bar@(BarIndex sid typ bari) =
  case typ of
       ColumnType -> Just bar
       RowType | bari == r' ->  Nothing
                  | otherwise -> Just $ BarIndex sid typ (if bari >= r' then bari-1 else bari)

barIndexMap (DragCol oldC newC) bar@(BarIndex sid typ bari) =
  case typ of
       ColumnType
         | bari < min oldC newC -> Just bar
         | bari > max oldC newC -> Just bar
         | bari == oldC         -> Just $ BarIndex sid typ newC
         | oldC < newC       -> Just $ BarIndex sid typ (bari-1) -- here on we assume c is strictly between oldC and newC
         | oldC > newC       -> Just $ BarIndex sid typ (bari+1)
       RowType -> Just bar
barIndexMap (DragRow oldR newR) bar@(BarIndex sid typ bari) =
  case typ of
       ColumnType -> Just bar
       RowType
         | bari < min oldR newR -> Just bar
         | bari > max oldR newR -> Just bar
         | bari == oldR         -> Just $ BarIndex sid typ newR
         | oldR < newR       -> Just $ BarIndex sid typ (bari-1)-- here on we assume r is strictly between oldR and newR
         | oldR > newR       -> Just $ BarIndex sid typ (bari+1)
  -- case oldR == newR can't happen because oldR < r < newR since third pattern-match

-- #needsrefactor can probably be condensed with lenses somehow? 
barMap :: MutateType -> Bar -> Maybe Bar
barMap mt (Bar bInd bProps) = fmap (\bInd' -> Bar bInd' bProps) (barIndexMap mt bInd)

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
  | oldR < newR       = Just $ Index sid (c, r-1) -- here on we assume r is strictly between oldR and newR
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
    cell' = if fatCellGotMutated mt rk
      then DI.toDecoupled c
      else c { cellExpression = (cellExpression c) { cRangeKey = rk { keyIndex = fromJust $ cellLocMap mt (keyIndex rk) } } } 

between :: Int -> Int -> Int -> Bool
between lower upper x = (x >= lower) && (x <= upper)

fatCellGotMutated :: MutateType -> RangeKey -> Bool
fatCellGotMutated (InsertCol c) (RangeKey (Index _ (tlc, _)) dims) = between (tlc + 1) (tlc + (width dims) - 1) c
fatCellGotMutated (InsertRow r) (RangeKey (Index _ (_, tlr)) dims) = between (tlr + 1) (tlr + (height dims) - 1) r

fatCellGotMutated (DeleteCol c) (RangeKey (Index _ (tlc, _)) dims) = between tlc (tlc + (width dims) - 1) c
fatCellGotMutated (DeleteRow r) (RangeKey (Index _ (_, tlr)) dims) = between tlr (tlr + (height dims) - 1) r

fatCellGotMutated (DragCol c1 c2) (RangeKey (Index _ (tlc, _)) dims) = case (width dims) of 
  1 -> False -- if width of the dragged col area was 1, then nothing happened.
  _ -> or [
      between tlc (tlc + (width dims) - 1) c1
    , between (tlc + 1) (tlc + (width dims) - 1) c2 
    ]

fatCellGotMutated (DragRow r1 r2) (RangeKey (Index _ (_, tlr)) dims) = case (height dims) of 
  1 -> False -- if height of the dragged row area was 1, then nothing happened.
  _ -> or [ 
      between tlr (tlr + (height dims) - 1) r1 
    , between (tlr + 1) (tlr + (height dims) - 1) r2 
    ]
