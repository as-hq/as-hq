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
import Control.Monad
import Control.Lens
import Data.Maybe

handleMutateSheet :: MessageId -> ASUserClient -> MVar ServerState -> MutateType -> IO ()
handleMutateSheet mid uc state mutateType = do
  let sid = userSheetId uc
  conn <- view dbConn <$> readMVar state
  -- update cells 
  oldCells <- DB.getCellsInSheet conn (userSheetId uc)
--   printObj "OLD CELLS: " oldCells
  let newCells = map (cellMutate mutateType) oldCells
      (oldCells', newCells') = keepUnequal $ zip oldCells newCells
      blankedCells = blankCellsAt $ mapCellLocation oldCells'
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
  broadcastErrOrUpdate mid state uc errOrUpdate

keepUnequal :: (Eq a) => [(a, Maybe a)] -> ([a], [a])
keepUnequal x = (ls1, ls2)
  where
    unequals = filter (\(c, c') -> (Just c /= c')) x
    ls1 = map      fst unequals
    ls2 = mapMaybe snd unequals

-- Functions from colMutate and rowMutate are helper functions for barIndexMutate,
--
-- indexMutate, exIndexMutate, exRangeMutate.
-- TODO: timchu 1/1/16. Refactor MutateType so that Inserts take two arguments: barType and bar number.
-- This is to reduce code duplication, and because conceptually insert, drag, delete
-- are the same thing: operations on a one dimensional list. Once this is updated,
-- colMutate and rowMutate should change. This code as is is a temporary hack.
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

coordMutate :: MutateType -> Coord -> Maybe Coord
coordMutate mt = (col %%~ colMutate mt) >=> (row %%~ rowMutate mt)

-- #lenses
indexMutate :: MutateType -> (ASIndex -> Maybe ASIndex)
indexMutate mt (Index sid coord) = (Index sid) <$> (coordMutate mt coord)

exIndexMutate :: MutateType -> ExIndex -> Maybe ExIndex
exIndexMutate mt (ExIndex indexRefType cStr rStr) = do
  cStr' <- intToColStr <$> (colMutate mt $ colStrToInt cStr)
  rStr' <- intToRowStr <$> (rowMutate mt $ rowStrToInt rStr)
  return $ ExIndex indexRefType cStr' rStr'

exColMutate :: MutateType -> ExCol -> Maybe ExCol
exColMutate mt (ExCol singleRefType cStr) = do
  cStr' <- intToColStr <$> (colMutate mt $ colStrToInt cStr)
  return $ ExCol singleRefType cStr'

-- | timchu, 1/1/16. Many of the below are helper functions used for range mutation, in the case where
-- the br is deleted but the tl is not.

-- TODO: timchu, 1/1/16. The type safety on these methods is terrible. 
-- TODO: Refactor Row and Col to be separate types to avoid this kind of rap.
shiftColStrLeft :: String -> String
shiftColStrLeft col = intToColStr $ (colStrToInt col) - 1

shiftRowStrUp :: String -> String
shiftRowStrUp row = intToRowStr $ (rowStrToInt row) - 1

shiftExIndexUp :: ExIndex -> ExIndex
shiftExIndexUp (ExIndex refType col row) = ExIndex refType col (shiftRowStrUp row)

shiftExIndexLeft :: ExIndex -> ExIndex
shiftExIndexLeft (ExIndex refType col row) = ExIndex refType (shiftColStrLeft col) row

exRangeMutate :: MutateType -> ExRange -> Maybe ExRange
exRangeMutate mt exRange =
  -- force t <= b. l <= r.
  let ExRange tl br = orientExRange exRange
      [maybeTl, maybeBr] = map (exIndexMutate mt) [tl, br]
  in
  -- cases on whether Tl or Br, or both, are deleted.
  case (maybeTl, maybeBr) of
    (Nothing, Nothing) -> Nothing
    (Nothing, Just br') -> Just $ ExRange tl br'
    (Just tl', Nothing) -> case mt of -- in this case, tl' should be equal to tl
                             (DeleteRow _) -> Just $ ExRange tl' $ shiftExIndexUp br
                             (DeleteCol _) -> Just $ ExRange tl' $ shiftExIndexLeft br
                             otherwise -> Nothing
    (Just tl', Just br') -> Just $ ExRange tl' br'

-- This is secretly the same as exRangeMutate with an infinite bottom row.
exColRangeMutate :: MutateType -> ExColRange -> Maybe ExColRange
exColRangeMutate mt exColRange =
  -- force l <= r.
  let ExColRange tl r = orientExColRange exColRange
      maybeTl = exIndexMutate mt tl
      maybeR = exColMutate mt r
      -- shiftColLeft is used to handle the case when r is deleted by tl isn't.
      shiftColLeft :: ExCol -> ExCol
      shiftColLeft (ExCol srType colStr) = ExCol srType (shiftColStrLeft colStr)
  in
  -- cases on whether tl, or r, or both, are deleted.
  case (maybeTl, maybeR) of
    (Nothing, Nothing) -> Nothing
    (Nothing, Just r') -> Just $ ExColRange tl r'
    (Just tl', Nothing) -> Just $ ExColRange tl' (shiftColLeft r) -- in this case, tl' should equal tl
    (Just tl', Just r') -> Just $ ExColRange tl' r'

-- returns Nothing if any of the mutations give out of bounds.
refMutate' :: MutateType -> (ExRef -> Maybe ExRef)
refMutate' mt ExOutOfBounds = Nothing
refMutate' mt (ExIndexRef exIndex sheetName workbookName) = do
  exIndex' <- exIndexMutate mt exIndex
  return $ ExIndexRef exIndex' sheetName workbookName
refMutate' mt (ExRangeRef exRange sheetName workbookName) = do
  exRange' <- exRangeMutate mt exRange
  return $ ExRangeRef exRange' sheetName workbookName
refMutate' mt (ExColRangeRef exColRange sheetName workbookName) = do
  exColRange' <- exColRangeMutate mt exColRange
  return $ ExColRangeRef exColRange' sheetName workbookName
  -- TODO: timchu, 1/1/16.  I don't know if this code works for pointers.
refMutate' mt er@(ExPointerRef exIndex sheetName workbookName) = do
  exIndex' <- exIndexMutate mt exIndex
  return $ ExPointerRef exIndex' sheetName workbookName

refMutate :: MutateType -> ExRef -> ExRef
refMutate mt exRef  = fromMaybe ExOutOfBounds $ refMutate' mt exRef

expressionMutate :: MutateType -> (ASExpression -> ASExpression)
expressionMutate mt = replaceRefs (show . (refMutate mt))

cellMutate :: MutateType -> (ASCell -> Maybe ASCell)
cellMutate mt c = case indexMutate mt $ c^.cellLocation of
   Nothing -> Nothing
   Just loc' -> Just $ sanitizeMutateCell mt loc' $ c & cellExpression %~ expressionMutate mt & cellLocation .~loc'

-- | If the cell passed in is uncoupled, leave it as is. Otherwise:
-- * if its range would get decoupled by the mutation, decouple it.
-- * if not, if it is a fat cell head, make it an uncoupled cell that'll get re-evaled.
-- * if it is not a fat cell head, delete it.
-- whether the fatcell it was a part of would get split up by the type of mutation.
sanitizeMutateCell :: MutateType -> ASIndex -> ASCell -> ASCell
sanitizeMutateCell _ _ c@(Cell { _cellRangeKey = Nothing }) = c
sanitizeMutateCell mt oldLoc c = cell'
  where
    Just rk = c^.cellRangeKey
    cell' = if fatCellGotMutated mt rk
      then DI.toDecoupled c
      else c & cellRangeKey .~ Just rk { keyIndex = fromJust $ indexMutate mt (keyIndex rk) }  -- #lens

between :: Int -> Int -> Int -> Bool
between lower upper x = (x >= lower) && (x <= upper)

fatCellGotMutated :: MutateType -> RangeKey -> Bool
fatCellGotMutated (InsertCol c) (RangeKey (Index _ coord) dims) = between (tlc + 1) (tlc + (width dims) - 1) c
  where tlc = coord^.col
fatCellGotMutated (InsertRow r) (RangeKey (Index _ coord) dims) = between (tlr + 1) (tlr + (height dims) - 1) r
  where tlr = coord^.row
fatCellGotMutated (DeleteCol c) (RangeKey (Index _ coord) dims) = between tlc (tlc + (width dims) - 1) c
  where tlc = coord^.col
fatCellGotMutated (DeleteRow r) (RangeKey (Index _ coord) dims) = between tlr (tlr + (height dims) - 1) r
  where tlr = coord^.row

fatCellGotMutated (DragCol c1 c2) (RangeKey (Index _ coord) dims) = case (width dims) of
  1 -> False -- if width of the dragged col area was 1, then happeneds.
  _ -> or [
      between tlc (tlc + (width dims) - 1) c1
    , between (tlc + 1) (tlc + (width dims) - 1) c2
    ]
  where tlc = coord^.col

fatCellGotMutated (DragRow r1 r2) (RangeKey (Index _ coord) dims) = case (height dims) of
  1 -> False -- if height of the dragged row area was 1, then nothing happened.
  _ -> or [
      between tlr (tlr + (height dims) - 1) r1
    , between (tlr + 1) (tlr + (height dims) - 1) r2
    ]
  where tlr = coord^.row

-- #incomplete not actually correct for dragging columns; in sheets, if we drag B to F, and the range was from A1:D4,
-- the new ranges become A1:A4, B1:C4, and F1:F4 (or something like that).
-- #ExposedConstructor : Coord
rangeMutate :: MutateType -> (ASRange -> [ASRange])
rangeMutate mt@(DeleteCol c) rng@(Range sid (Coord tlCol tlRow, Coord blCol blRow))
-- Question, timchu, 12/29/15. We seem to allow improperly oriented ranges!
  | tlCol > blCol            = error "improperly oriented range passed into rangeMutate"
  | tlCol == c && blCol == c = []
  | tlCol == c             = [Range sid (Coord tlCol tlRow, Coord (blCol-1) blRow)] -- not (Coord tlCol+1 tlRow, Coord blCol blRow) since the cols gets shifted
  | blCol == c             = [Range sid (Coord tlCol tlRow, Coord (blCol-1) blRow)]
  | otherwise            = rangeMutate' mt rng
rangeMutate mt@(DeleteRow r) rng@(Range sid (Coord tlCol tlRow, Coord blCol blRow))
  | tlRow > blRow            = error "improperly oriented range passed into rangeMutate"
  | tlRow == r && blRow == r = []
  | tlRow == r             = [Range sid (Coord tlCol tlRow, Coord blCol (blRow-1))]
  | blRow == r             = [Range sid (Coord tlCol tlRow, Coord blCol (blRow-1))]
  | otherwise            = rangeMutate' mt rng
rangeMutate mt rng = rangeMutate' mt rng

-- Assumes none of the boundaries of the range got deleted, in which case the indexMutate's should
-- never be Nothing.
rangeMutate' :: MutateType -> (ASRange -> [ASRange])
rangeMutate' mt (Range sid (c1, c2)) = [orientRange $ Range sid (c1', c2')]
  where
    Just (Index _ c1') = indexMutate mt (Index sid c1)
    Just (Index _ c2') = indexMutate mt (Index sid c2)


condFormattingRulesMutate :: MutateType -> CondFormatRule -> Maybe CondFormatRule
condFormattingRulesMutate mt cfr = cfr'
  where
    cellLocs' = concatMap (rangeMutate mt) (cellLocs cfr)
    formatMapConstructor' = condFormatConditionMutate mt (formatMapConstructor cfr)
    cfr' = if null cellLocs' then Nothing else Just $ cfr { cellLocs = cellLocs', formatMapConstructor = formatMapConstructor' }

condFormatConditionMutate :: MutateType -> FormatMapConstructor -> FormatMapConstructor
condFormatConditionMutate mt (BoolFormatMapConstructor boolCond prop) = BoolFormatMapConstructor (boolCondFormatConditionMutate mt boolCond) prop

boolCondFormatConditionMutate :: MutateType -> BoolCondition -> BoolCondition
boolCondFormatConditionMutate mt (CustomBoolCond xp) = CustomBoolCond (expressionMutate mt xp)
boolCondFormatConditionMutate mt (NoExprBoolCond typ) = NoExprBoolCond typ
boolCondFormatConditionMutate mt (OneExprBoolCond typ xp) = OneExprBoolCond typ (expressionMutate mt xp)
boolCondFormatConditionMutate mt (TwoExprBoolCond typ xp1 xp2) = TwoExprBoolCond typ (expressionMutate mt xp1) (expressionMutate mt xp2)
