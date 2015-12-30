module AS.Handlers.Mutate (handleMutateSheet) where

import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User
import AS.Types.Excel hiding (dbConn)
import AS.Types.Eval
import AS.Types.Commits
import AS.Types.Updates
import AS.Types.CondFormat

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

handleMutateSheet :: ASUserClient -> MVar ServerState -> MutateType -> IO ()
handleMutateSheet uc state mutateType = do
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
      bu = Update { oldKeys = map barIndex oldBars', newVals = newBars' }
  -- update conditional formatting rules
  oldCondFormatRules <- DB.getCondFormattingRulesInSheet conn sid
  let newCondFormatRules = map (condFormattingRulesMap mutateType) oldCondFormatRules
      (oldCondFormatRules', newCondFormatRules') = keepUnequal $ zip oldCondFormatRules newCondFormatRules
      cfru = Update { oldKeys = map condFormatRuleId oldCondFormatRules', newVals = newCondFormatRules' }
  -- propagate changes
  let updateTransform = \update -> update { barUpdates = bu, condFormatRulesUpdates = cfru }
  errOrUpdate <- runDispatchCycle state updatedCells DescendantsWithParent (userCommitSource uc) updateTransform
  broadcastErrOrUpdate state uc errOrUpdate

keepUnequal :: (Eq a) => [(a, Maybe a)] -> ([a], [a])
keepUnequal x = (ls1, ls2) 
  where 
    unequals = filter (\(c, c') -> (Just c /= c')) x
    ls1 = map      fst unequals
    ls2 = mapMaybe snd unequals

-- #lenses
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

-- #lens
barMap :: MutateType -> Bar -> Maybe Bar
barMap mt (Bar bInd bProps) = fmap (\bInd' -> Bar bInd' bProps) (barIndexMap mt bInd)

indexMap :: MutateType -> (ASIndex -> Maybe ASIndex)
indexMap (InsertCol c') (Index sid (c, r)) = Just $ Index sid (if c >= c' then c+1 else c, r)
indexMap (InsertRow r') (Index sid (c, r)) = Just $ Index sid (c, if r >= r' then r+1 else r)
indexMap (DeleteCol c') i@(Index sid (c, r))
  | c == c'  = Nothing
  | c > c'   = Just $ Index sid (c-1, r)
  | c < c'   = Just i
indexMap (DeleteRow r') i@(Index sid (c, r))
  | r == r'  = Nothing
  | r > r'   = Just $ Index sid (c, r-1)
  | r < r'   = Just i
indexMap (DragCol oldC newC) i@(Index sid (c, r)) -- DragCol 3 1 : (123) -> (312)
  | c < min oldC newC = Just i
  | c > max oldC newC = Just i
  | c == oldC         = Just $ Index sid (newC, r) 
  | oldC < newC       = Just $ Index sid (c-1, r) -- here on we assume c is strictly between oldC and newC
  | oldC > newC       = Just $ Index sid (c+1, r)
  -- case oldC == newC can't happen because oldC < c < newC since third pattern-match
indexMap (DragRow oldR newR) i@(Index sid (c, r))
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
    er' = case (indexMap mt ind) of 
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
cellMap mt c@(Cell loc xp v ps) = case ((indexMap mt) loc) of 
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
      else c { cellExpression = (cellExpression c) { cRangeKey = rk { keyIndex = fromJust $ indexMap mt (keyIndex rk) } } } 

between :: Int -> Int -> Int -> Bool
between lower upper x = (x >= lower) && (x <= upper)

fatCellGotMutated :: MutateType -> RangeKey -> Bool
fatCellGotMutated (InsertCol c) (RangeKey (Index _ (tlc, _)) dims) = between (tlc + 1) (tlc + (width dims) - 1) c
fatCellGotMutated (InsertRow r) (RangeKey (Index _ (_, tld)) dims) = between (tld + 1) (tld + (height dims) - 1) r

fatCellGotMutated (DeleteCol c) (RangeKey (Index _ (tlc, _)) dims) = between tlc (tlc + (width dims) - 1) c
fatCellGotMutated (DeleteRow r) (RangeKey (Index _ (_, tld)) dims) = between tld (tld + (height dims) - 1) r

fatCellGotMutated (DragCol c1 c2) (RangeKey (Index _ (tlc, _)) dims) = case (width dims) of 
  1 -> False -- if width of the dragged col area was 1, then happeneds.
  _ -> or [
      between tlc (tlc + (width dims) - 1) c1
    , between (tlc + 1) (tlc + (width dims) - 1) c2 
    ]

fatCellGotMutated (DragRow r1 r2) (RangeKey (Index _ (_, tld)) dims) = case (height dims) of 
  1 -> False -- if height of the dragged row area was 1, then nothing happened.
  _ -> or [ 
      between tld (tld + (height dims) - 1) r1 
    , between (tld + 1) (tld + (height dims) - 1) r2 
    ]

-- #incomplete not actually correct for dragging columns; in sheets, if we drag B to F, and the range was from A1:D4, 
-- the new ranges become A1:A4, B1:C4, and F1:F4 (or something like that). 
rangeMap :: MutateType -> (ASRange -> [ASRange])
rangeMap mt@(DeleteCol c) rng@(Range sid ((tlc, tlr), (blc, blr)))
  | tlc > blc            = error "improperly oriented range passed into rangeMap"
  | tlc == c && blc == c = []
  | tlc == c             = [Range sid ((tlc, tlr), (blc-1, blr))] -- not ((tlc+1, tlr), (blc, blr)) since the cols gets shifted
  | blc == c             = [Range sid ((tlc, tlr), (blc-1, blr))]
  | otherwise            = rangeMap' mt rng
rangeMap mt@(DeleteRow r) rng@(Range sid ((tlc, tlr), (blc, blr)))
  | tlr > blr            = error "improperly oriented range passed into rangeMap"
  | tlr == r && blr == r = [] 
  | tlr == r             = [Range sid ((tlc, tlr), (blc, blr-1))]
  | blr == r             = [Range sid ((tlc, tlr), (blc, blr-1))]
  | otherwise            = rangeMap' mt rng
rangeMap mt rng = rangeMap' mt rng

-- Assumes none of the boundaries of the range got deleted, in which case the indexMap's should
-- never be Nothing. 
rangeMap' :: MutateType -> (ASRange -> [ASRange])
rangeMap' mt (Range sid (c1, c2)) = [orientRange $ Range sid (c1', c2')]
  where
    Just (Index _ c1') = indexMap mt (Index sid c1)
    Just (Index _ c2') = indexMap mt (Index sid c2)

-- #lens
condFormattingRulesMap :: MutateType -> CondFormatRule -> Maybe CondFormatRule
condFormattingRulesMap mt cfr = cfr'
  where 
    cellLocs' = concatMap (rangeMap mt) (cellLocs cfr)
    condition' = condFormatConditionMap mt (condition cfr)
    cfr' = if null cellLocs' then Nothing else Just $ cfr { cellLocs = cellLocs', condition = condition' }

-- #lens
condFormatConditionMap :: MutateType -> CondFormatCondition -> CondFormatCondition
condFormatConditionMap mt (CustomCondition (Custom xp)) = CustomCondition $ Custom (expressionMap mt xp)
condFormatConditionMap mt (IsEmptyCondition IsEmpty) = IsEmptyCondition IsEmpty
condFormatConditionMap mt (IsNotEmptyCondition IsNotEmpty) = IsNotEmptyCondition IsNotEmpty
condFormatConditionMap mt (GreaterThanCondition (GreaterThan xp)) = GreaterThanCondition $ GreaterThan (expressionMap mt xp) 
condFormatConditionMap mt (LessThanCondition (LessThan xp)) = LessThanCondition $ LessThan (expressionMap mt xp) 
condFormatConditionMap mt (GeqCondition (Geq xp)) = GeqCondition $ Geq (expressionMap mt xp) 
condFormatConditionMap mt (LeqCondition (Leq xp)) = LeqCondition $ Leq (expressionMap mt xp) 
condFormatConditionMap mt (EqualsCondition (Equals xp)) = EqualsCondition $ Equals (expressionMap mt xp) 
condFormatConditionMap mt (NotEqualsCondition (NotEquals xp)) = NotEqualsCondition $ NotEquals (expressionMap mt xp) 
condFormatConditionMap mt (IsBetweenCondition (IsBetween xp1 xp2)) = IsBetweenCondition $ IsBetween (expressionMap mt xp1) (expressionMap mt xp2)
condFormatConditionMap mt (IsNotBetweenCondition (IsNotBetween xp1 xp2)) = IsNotBetweenCondition $ IsNotBetween (expressionMap mt xp1) (expressionMap mt xp2)
