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
import Control.Lens
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
  broadcastErrOrUpdate state uc errOrUpdate

keepUnequal :: (Eq a) => [(a, Maybe a)] -> ([a], [a])
keepUnequal x = (ls1, ls2)
  where
    unequals = filter (\(c, c') -> (Just c /= c')) x
    ls1 = map      fst unequals
    ls2 = mapMaybe snd unequals

-- Functions from colMutate and rowMutate are helper functions for barIndexMutate,
-- indexMutate, exIndexMutate, exRangeMutate.
-- TODO: timchu 1/1/15, could have better typing on rows and columns so that
-- compiler can catch accidental mistakes when switching col and row.
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
    r' <- rowMutate mt r
    return $ Index sid (c', r')

rowStrToInt :: String -> Int
rowStrToInt r = read r :: Int

intToRowStr :: Int -> String
intToRowStr i = show i

exIndexMutate :: MutateType -> ExLoc -> Maybe ExLoc
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

-- TODO: timchu, 1/1/16. The type safety on these methods is not great.
shiftColStrLeft :: String -> String
shiftColStrLeft col = intToColStr $ (colStrToInt col) - 1

shiftRowStrUp :: String -> String
shiftRowStrUp row = intToRowStr $ (rowStrToInt row) - 1

shiftExIndexUp :: ExLoc -> ExLoc
shiftExIndexUp (ExIndex refType col row) = ExIndex refType col (shiftRowStrUp row)

shiftExIndexLeft :: ExLoc -> ExLoc
shiftExIndexLeft (ExIndex refType col row) = ExIndex refType (shiftColStrLeft col) row

-- Helper methods in rectifyExRange
-- TODO: timchu, 1/3/15. Relocate functions. This should be extraneous once RefType
-- becomes a pair of singleRefTypes.
splitRefType :: RefType -> (SingleRefType,  SingleRefType)
splitRefType rType =
  case rType of
       REL_REL -> (REL, REL)
       REL_ABS -> (REL, ABS)
       ABS_REL -> (ABS, REL)
       ABS_ABS -> (ABS, ABS)
combineSingleRefs :: (SingleRefType,  SingleRefType) -> RefType
combineSingleRefs srTypes =
  case srTypes of
      (REL, REL) -> REL_REL 
      (REL, ABS) -> REL_ABS 
      (ABS, REL) -> ABS_REL 
      (ABS, ABS) -> ABS_ABS 
-- outputs an exRange equivalent to the input of the first ExRange, with the first coord <= second coord
-- #lenses
-- Lots of code duplication betwen rectifyExRange and rectifyExColRange
rectifyExRange :: ExRange -> ExRange
rectifyExRange e@(ExRange (ExIndex firstRefType firstCol firstRow) (ExIndex secondRefType secondCol secondRow)) =
  let (firstColRefType, firstRowRefType) = splitRefType firstRefType
      (secondColRefType, secondRowRefType) = splitRefType secondRefType
      (l, lRefType, r, rRefType) = if firstCol <= secondCol
                  then (firstCol, firstColRefType, secondCol, secondColRefType)
                  else (secondCol, secondColRefType, firstCol, firstColRefType)
      (t, tRefType, b, bRefType) = if firstRow <= secondRow
                  then (firstRow, firstRowRefType, secondRow, secondRowRefType)
                  else (secondRow, secondRowRefType, firstRow, firstRowRefType)
      tlRefType = combineSingleRefs (lRefType, tRefType)
      brRefType = combineSingleRefs (rRefType, bRefType)
  in 
  -- l t and r bsince Col comes before Row in ExIndex
  ExRange (ExIndex tlRefType l t) (ExIndex brRefType r b)

-- outputs an exColRange equivalent to the input of the first ExColRange, with the first coord <= second coord
rectifyExColRange :: ExColRange -> ExColRange
rectifyExColRange e@(ExColRange (ExIndex firstRefType firstCol firstRow) (ExCol secondColRefType secondCol)) =
  let t = firstRow
      (firstColRefType, tRefType) = splitRefType firstRefType
      (l, lRefType, r, rRefType) = if firstCol <= secondCol
                  then (firstCol, firstColRefType, secondCol, secondColRefType)
                  else (secondCol, secondColRefType, firstCol, firstColRefType)
      tlRefType = combineSingleRefs (lRefType, tRefType)
  in 
  ExColRange (ExIndex tlRefType l t) (ExCol rRefType r)

exRangeMutate :: MutateType -> ExRange -> Maybe ExRange
exRangeMutate mt exRange =
  -- force t <= b. l <= r.
  let ExRange tl br = rectifyExRange exRange
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

-- timchu, 1/1/2016. Only works if l <= r.
-- Note: this will cause problems since this operation does not preserve l <= r
-- r the name for the rightmost column.
exColRangeMutate :: MutateType -> ExColRange -> Maybe ExColRange
exColRangeMutate mt exColRange =
  let ExColRange tl r = rectifyExColRange exColRange
      maybeTl = exIndexMutate mt tl
      maybeR = exColMutate mt r
      -- shiftColLeft is used to handle the case when r is deleted by tl isn't.
      shiftColLeft :: ExCol -> ExCol
      shiftColLeft (ExCol srType colStr) = ExCol srType (shiftColStrLeft colStr)
  in
  -- cases on whether Tl, or r, or both, are deleted.
  case (maybeTl, maybeR) of
    (Nothing, Nothing) -> Nothing
    (Nothing, Just r') -> Just $ ExColRange tl r'
    (Just tl', Nothing) -> Just $ ExColRange tl' (shiftColLeft r) -- in this case, tl' should equal tl
    (Just tl', Just r') -> Just $ ExColRange tl' r'

-- returns Nothing if any of the mutations give out of bounds.
refMutate' :: MutateType -> (ExRef -> Maybe ExRef)
refMutate' mt ExOutOfBounds = Nothing
refMutate' mt (ExLocRef exLoc sheetName workbookName) = do
  exLoc' <- exIndexMutate mt exLoc
  return $ ExLocRef exLoc' sheetName workbookName
refMutate' mt (ExRangeRef exRange sheetName workbookName) = do
  exRange' <- exRangeMutate mt exRange
  return $ ExRangeRef exRange' sheetName workbookName
refMutate' mt (ExColRangeRef exColRange sheetName workbookName) = do
  exColRange' <- exColRangeMutate mt exColRange
  return $ ExColRangeRef exColRange' sheetName workbookName
  -- TODO: timchu, 1/1/16.  I don't know if this code works for pointers.
refMutate' mt er@(ExPointerRef exLoc sheetName workbookName) = do
  exLoc' <- exIndexMutate mt exLoc
  return $ ExPointerRef exLoc' sheetName workbookName

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
