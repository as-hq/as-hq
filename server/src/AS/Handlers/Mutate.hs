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
--   printObj "OLD CELLS: " oldCells
  let newCells = map (cellMutate mutateType) oldCells
      (oldCells', newCells') = keepUnequal $ zip oldCells newCells
      blankedCells = blankCellsAt $ map cellLocation oldCells'
      updatedCells = mergeCells newCells' blankedCells -- eval blanks at the old cell locations, re-eval at new locs
--  printObj "NEW CELLS: " newCells
  -- update barProps
  oldBars <- DB.getBarsInSheet conn sid
  let newBars = map (barMutate mutateType) oldBars
      (oldBars', newBars') = keepUnequal $ zip oldBars newBars
      bu = Update { oldKeys = map barIndex oldBars', newVals = newBars' }
  -- update conditional formatting rules
  oldCondFormatRules <- DB.getCondFormattingRulesInSheet conn sid
  let newCondFormatRules = map (condFormattingRulesMutate mutateType) oldCondFormatRules
      (oldCondFormatRules', newCondFormatRules') = keepUnequal $ zip oldCondFormatRules newCondFormatRules
      cfru = Update { oldKeys = map condFormatRuleId oldCondFormatRules', newVals = newCondFormatRules' }
  -- propagate changes
  let updateTransform update = update { barUpdates = bu, condFormatRulesUpdates = cfru }
  errOrUpdate <- runDispatchCycle state updatedCells DescendantsWithParent (userCommitSource uc) updateTransform
  broadcastErrOrUpdate state uc errOrUpdate

keepUnequal :: (Eq a) => [(a, Maybe a)] -> ([a], [a])
keepUnequal x = (ls1, ls2)
  where
    unequals = filter (\(c, c') -> (Just c /= c')) x
    ls1 = map      fst unequals
    ls2 = mapMaybe snd unequals

-- Helper functions for barIndex and index mutate.
colMutate :: MutateType -> Col -> Maybe Col
colMutate (InsertCol c') c = Just $ if c >= c' then (c+1) else c
colMutate (DeleteCol c') c
  | c == c'  = Nothing
  | c > c'   = Just $ c-1
  | c < c'   = Just c
colMutate (DragCol oldC newC) c -- DragCol 3 1 : (123) -> (312)
  | c < min oldC newC = Just c
  | c > max oldC newC = Just c
  | c == oldC         = Just newC
  | oldC < newC       = Just $ c-1
  | oldC > newC       = Just $ c+1
  -- case oldC == newC can't happen because oldC < c < newC since third pattern-match
colMutate _ c = Just c

rowMutate :: MutateType -> Row -> Maybe Row
rowMutate (InsertRow r') r = Just $ if r >= r' then (r+1) else r
rowMutate (DeleteRow r') r
  | r == r'  = Nothing
  | r >  r'   = Just $ r-1
  | r <  r'   = Just r
rowMutate (DragRow oldR newR) r -- DragRow 3 1 : (123) -> (312)
  | r < min oldR newR = Just r
  | r > max oldR newR = Just r
  | r == oldR         = Just newR
  | oldR < newR       = Just $ r-1
  | oldR > newR       = Just $ r+1
  -- case oldR == newR can't happen because oldR < r < newR since third pattern-match
rowMutate _ r = Just r

-- #lenses
-- | For a mutate, maps the old row and column to the new row and column.
barIndexMutate :: MutateType -> BarIndex -> Maybe BarIndex
barIndexMutate mt bar@(BarIndex sid typ bari) = do
  bari' <- case typ of
             ColumnType -> colMutate mt bari
             RowType -> rowMutate mt bari
  return $ BarIndex sid typ bari'

-- #lens
barMutate :: MutateType -> Bar -> Maybe Bar
barMutate mt (Bar bInd bProps) = fmap (`Bar` bProps) (barIndexMutate mt bInd)

indexMutate :: MutateType -> (ASIndex -> Maybe ASIndex)
indexMutate mt (Index sid (c, r)) = do
    c' <- colMutate mt c
    r' <- colMutate mt r
    return $ Index sid (c', r')

refMutate :: MutateType -> (ExRef -> ExRef)
refMutate mt ExOutOfBounds = ExOutOfBounds
refMutate mutateType exLocRef@(ExLocRef (ExIndex indexRefType _ _) sheetName workbookName) = exLocRef'
  where -- feels kinda ugly...
    IndexRef ind = exRefToASRef (T.pack "") exLocRef
    exLocRef' = case (indexMutate mutateType ind) of
      Nothing -> ExOutOfBounds
      Just newRefLoc -> ExLocRef exIndex' sheetName workbookName
        where
          ExLocRef exIndex _ _ = asRefToExRef $ IndexRef newRefLoc
          exIndex' = exIndex { refType = indexRefType }

-- TODO: timchu, 12/29/15. Problem if ExLocRef is ExOutOfBounds errors. Currently using rangeMutate instead?
refMutate mutateType er@(ExRangeRef (ExRange tl br) sheetName workbookName) = ExRangeRef (ExRange tl' br') sheetName workbookName
  where
    ExLocRef tl' _ _ = refMutate mutateType (ExLocRef tl sheetName workbookName)
    ExLocRef br' _ _ = refMutate mutateType (ExLocRef br sheetName workbookName)
-- TODO: timchu, 12/29/15. Problem if colMutate returns a nothing. Use colRangeMutate instead?
refMutate mutateType er@(ExPointerRef exLoc sheetName workbookName) = ExPointerRef exLoc' sheetName workbookName
  where
    ExLocRef exLoc' _ _ = refMutate mutateType (ExLocRef exLoc sheetName workbookName)

expressionMutate :: MutateType -> (ASExpression -> ASExpression)
expressionMutate mt = replaceRefs (show . (refMutate mt))

cellMutate :: MutateType -> (ASCell -> Maybe ASCell)
cellMutate mt c@(Cell loc xp v ps) = case ((indexMutate mt) loc) of
  Nothing -> Nothing
  Just loc' -> Just $ sanitizeMutateCell mt loc $ Cell loc' ((expressionMutate mt) xp) v ps

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
      else c { cellExpression = (cellExpression c) { cRangeKey = rk { keyIndex = fromJust $ indexMutate mt (keyIndex rk) } } }

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
rangeMutate :: MutateType -> (ASRange -> [ASRange])
rangeMutate mt@(DeleteCol c) rng@(Range sid ((tlCol, tlRow), (blCol, blRow)))
-- #Question, timchu, 12/29/15. We seem to allow improperly oriented ranges!
  | tlCol > blCol            = error "improperly oriented range passed into rangeMutate"
  | tlCol == c && blCol == c = []
  | tlCol == c             = [Range sid ((tlCol, tlRow), (blCol-1, blRow))] -- not ((tlCol+1, tlRow), (blCol, blRow)) since the cols gets shifted
  | blCol == c             = [Range sid ((tlCol, tlRow), (blCol-1, blRow))]
  | otherwise            = rangeMutate' mt rng
rangeMutate mt@(DeleteRow r) rng@(Range sid ((tlCol, tlRow), (blCol, blRow)))
  | tlRow > blRow            = error "improperly oriented range passed into rangeMutate"
  | tlRow == r && blRow == r = []
  | tlRow == r             = [Range sid ((tlCol, tlRow), (blCol, blRow-1))]
  | blRow == r             = [Range sid ((tlCol, tlRow), (blCol, blRow-1))]
  | otherwise            = rangeMutate' mt rng
rangeMutate mt rng = rangeMutate' mt rng

-- Assumes none of the boundaries of the range got deleted, in which case the indexMutate's should
-- never be Nothing.
rangeMutate' :: MutateType -> (ASRange -> [ASRange])
rangeMutate' mt (Range sid (c1, c2)) = [orientRange $ Range sid (c1', c2')]
  where
    Just (Index _ c1') = indexMutate mt (Index sid c1)
    Just (Index _ c2') = indexMutate mt (Index sid c2)


--TODO: timchu, mutateColRange.
-- #lens
condFormattingRulesMutate :: MutateType -> CondFormatRule -> Maybe CondFormatRule
condFormattingRulesMutate mt cfr = cfr'
  where
    cellLocs' = concatMap (rangeMutate mt) (cellLocs cfr)
    condition' = condFormatConditionMutate mt (condition cfr)
    cfr' = if null cellLocs' then Nothing else Just $ cfr { cellLocs = cellLocs', condition = condition' }

-- #lens
condFormatConditionMutate :: MutateType -> CondFormatCondition -> CondFormatCondition
condFormatConditionMutate mt (CustomCondition (Custom xp)) = CustomCondition $ Custom (expressionMutate mt xp)
condFormatConditionMutate mt (IsEmptyCondition IsEmpty) = IsEmptyCondition IsEmpty
condFormatConditionMutate mt (IsNotEmptyCondition IsNotEmpty) = IsNotEmptyCondition IsNotEmpty
condFormatConditionMutate mt (GreaterThanCondition (GreaterThan xp)) = GreaterThanCondition $ GreaterThan (expressionMutate mt xp)
condFormatConditionMutate mt (LessThanCondition (LessThan xp)) = LessThanCondition $ LessThan (expressionMutate mt xp)
condFormatConditionMutate mt (GeqCondition (Geq xp)) = GeqCondition $ Geq (expressionMutate mt xp)
condFormatConditionMutate mt (LeqCondition (Leq xp)) = LeqCondition $ Leq (expressionMutate mt xp)
condFormatConditionMutate mt (EqualsCondition (Equals xp)) = EqualsCondition $ Equals (expressionMutate mt xp)
condFormatConditionMutate mt (NotEqualsCondition (NotEquals xp)) = NotEqualsCondition $ NotEquals (expressionMutate mt xp)
condFormatConditionMutate mt (IsBetweenCondition (IsBetween xp1 xp2)) = IsBetweenCondition $ IsBetween (expressionMutate mt xp1) (expressionMutate mt xp2)
condFormatConditionMutate mt (IsNotBetweenCondition (IsNotBetween xp1 xp2)) = IsNotBetweenCondition $ IsNotBetween (expressionMutate mt xp1) (expressionMutate mt xp2)
