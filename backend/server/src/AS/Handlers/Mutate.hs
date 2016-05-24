module AS.Handlers.Mutate (handleMutateSheet) where

import AS.Prelude

import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages hiding (Delete, Drag)
import AS.Types.User (UserID)
import AS.Types.Excel hiding (dbConn)
import AS.Types.Eval
import AS.Types.Commits
import AS.Types.Updates
import AS.Types.CondFormat
import AS.Types.Shift (shiftF)
import AS.Types.DataModification
import AS.Types.Mutate

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
import Data.Maybe hiding (fromJust)
import qualified Data.Set as S


handleMutateSheet :: MessageContext -> Mutate -> IO ()
handleMutateSheet msgctx mutateType = do
  let sid   = messageSheetId msgctx
      conn  = msgctx^.dbConnection
  -- update cells 
  oldCells <- DB.getCellsInSheet conn (messageSheetId msgctx)
  let newCells = map (cellMutate mutateType) oldCells
      (oldCells', newCells') = keepUnequal $ zip oldCells newCells
      blankedCells = blankCellsAt $ mapCellLocation oldCells'
      updatedCells = mergeCells newCells' blankedCells -- eval blanks at the old cell locations, re-eval at new locs
  -- update barProps
  oldBars <- DB.getBarsInSheet conn sid
  let newBars = map (barMutate mutateType) oldBars
      (oldBars', newBars') = keepUnequal $ zip oldBars newBars
      bu = updateFromLists newBars' $ map barIndex oldBars'
  -- update conditional formatting rules
  oldCondFormatRules <- DB.getCondFormattingRulesInSheet conn sid
  let newCondFormatRules = map (condFormattingRulesMutate mutateType) oldCondFormatRules
      (oldCondFormatRules', newCondFormatRules') = keepUnequal $ zip oldCondFormatRules newCondFormatRules
      cfru = updateFromLists newCondFormatRules' $ map condFormatRuleId oldCondFormatRules'
  -- propagate changes
  let updateTransform update = update & barUpdates .~ bu & condFormatRuleUpdate .~ cfru
  errOrUpdate <- runDispatchCycle msgctx updatedCells DescendantsWithParent updateTransform
  broadcastErrOrUpdate msgctx errOrUpdate

keepUnequal :: (Eq a) => [(a, Maybe a)] -> ([a], [a])
keepUnequal x = (ls1, ls2)
  where
    unequals = filter (\(c, c') -> (Just c /= c')) x
    ls1 = map      fst unequals
    ls2 = mapMaybe snd unequals

-- Mutates Col, Row.
coreMutate' :: (Num a, Ord a, Eq a) => MutateTypeNew a -> a -> Maybe a
coreMutate' (Insert v') v = Just $ if v' <= v then v+1 else v
coreMutate' (Delete v') v
  | v' == v  = Nothing
  | v' < v   = Just $ v-1
  | v' > v   = Just v
coreMutate' (Drag oldV newV) v -- Drag 3 1 : (123) --> (312)
  | min oldV newV > v = Just v
  | max oldV newV < v = Just v
  | oldV == v          = Just newV
  | oldV < newV       = Just $ v-1
  | oldV > newV       = Just $ v+1
-- oldV == newV van't happen because oldV < v < newV since third pattern-match

--- Mutates Col, Infinite Col, ExCol, ExRow, Infinite ExRow, Infinite ExCol, Bar,
--- by applying coreMutate mt on each of the underlying data types used to
--- construct each ADT.
-- #RoomForImprovement. Restrict coreMutate's parameter via typeclass.
coreMutate :: (Data a, Data b, Num a, Ord a, Eq a) => MutateTypeNew a -> b -> Maybe b
coreMutate mt = deepGMapM (coreMutate' mt)

-- mutates ExIndex, ExtendedExIndex, Coord, Extended Coord
coordMutate :: (Data a, Data b) => Mutate -> (a, b) -> Maybe (a, b)
coordMutate mt = case mt of
  ColMutate cm -> (& _1 %%~ coreMutate cm)
  RowMutate rm -> (& _2 %%~ coreMutate rm)

indexMutate :: Mutate -> ASIndex -> Maybe ASIndex
indexMutate mt ind = ind & index %%~ coordMutate mt

barMutate :: Mutate -> Bar -> Maybe Bar
barMutate mt =
  case mt of
    ColMutate cm -> coreMutate cm
    RowMutate rm -> coreMutate rm

--- Note: Timchu. This logic needs fixing. This should work like ASRange.
exRangeMutate :: Mutate -> ExRange -> Maybe ExRange
exRangeMutate mt exRange@(ExRange tl br) = do
  let maybeTl = coordMutate mt tl
      maybeBr = coordMutate mt br
  -- cases on whether Tl or Br, or both, are deleted.
  case (maybeTl, maybeBr) of
    (Nothing, Nothing) -> Nothing
    (Nothing, Just br') -> Just $ ExRange tl br'
    (Just tl', Nothing) -> case mt of -- in this case, tl' should be equal to tl
                             (RowMutate (Delete _)) -> Just $ ExRange tl' $ shiftF (Row $ -1 :: Row) br
                             (ColMutate (Delete _)) -> Just $ ExRange tl' $ shiftF (Col $ -1 :: Col) br
                             otherwise -> Nothing
    (Just tl', Just br') -> Just $ ExRange tl' br'


-- returns Nothing if any of the mutations give out of bounds.
refMutate' :: Mutate -> (ExRef -> Maybe ExRef)
refMutate' mt ExOutOfBounds = Nothing
refMutate' mt (ExIndexRef exIndex sheetName workbookName) = do
  exIndex' <- coordMutate mt exIndex
  return $ ExIndexRef exIndex' sheetName workbookName
refMutate' mt (ExRangeRef exRange sheetName workbookName) = do
  exRange' <- exRangeMutate mt exRange
  return $ ExRangeRef exRange' sheetName workbookName
refMutate' mt er@(ExPointerRef exIndex sheetName workbookName) = do
  exIndex' <- coordMutate mt exIndex
  return $ ExPointerRef exIndex' sheetName workbookName
refMutate' mt (ExTemplateRef t s w) = case t of 
  ExSampleExpr n exIndex -> do
    exIndex' <- coordMutate mt exIndex
    return $ ExTemplateRef (ExSampleExpr n exIndex') s w

refMutate :: Mutate -> ExRef -> ExRef
refMutate mt exRef  = fromMaybe ExOutOfBounds $ refMutate' mt exRef

expressionMutate :: Mutate -> (ASExpression -> ASExpression)
expressionMutate mt = replaceRefs (show . (refMutate mt))

cellMutate :: Mutate -> (ASCell -> Maybe ASCell)
cellMutate mt c = case indexMutate mt $ c^.cellLocation of
   Nothing -> Nothing
   Just loc' -> Just $ sanitizeMutateCell mt loc' $ c & cellExpression %~ expressionMutate mt & cellLocation .~loc'

-- | If the cell passed in is uncoupled, leave it as is. Otherwise:
-- * if its range would get decoupled by the mutation, decouple it.
-- * if not, if it is a fat cell head, make it an uncoupled cell that'll get re-evaled.
-- * if it is not a fat cell head, delete it.
-- whether the fatcell it was a part of would get split up by the type of mutation.
sanitizeMutateCell :: Mutate -> ASIndex -> ASCell -> ASCell
sanitizeMutateCell _ _ c@(Cell { _cellRangeKey = Nothing }) = c
sanitizeMutateCell mt oldLoc c = cell'
  where
    Just rk = c^.cellRangeKey
    cell' = if fatCellGotMutated mt rk
          then DI.toDecoupled c
          else let rk' = case indexMutate mt (keyIndex rk) of 
                            Nothing  -> Nothing
                            Just idx -> Just $ rk {keyIndex = idx}
                in c & cellRangeKey .~ rk'

between :: (Num a, Ord a) => a -> a -> a -> Bool
between lower upper x = (x >= lower) && (x <= upper)

fatCellGotMutated :: Mutate -> RangeKey -> Bool
fatCellGotMutated (ColMutate (Insert c)) (RangeKey (Index _ coord) dims) = between (tlc + 1) (tlc + (width dims) - 1) c
  where tlc = coord^.col
fatCellGotMutated (RowMutate (Insert r)) (RangeKey (Index _ coord) dims) = between (tlr + 1) (tlr + (height dims) - 1) r
  where tlr = coord^.row
fatCellGotMutated (ColMutate (Delete c)) (RangeKey (Index _ coord) dims) = between tlc (tlc + (width dims) - 1) c
  where tlc = coord^.col
fatCellGotMutated (RowMutate (Delete r)) (RangeKey (Index _ coord) dims) = between tlr (tlr + (height dims) - 1) r
  where tlr = coord^.row

fatCellGotMutated (ColMutate (Drag c1 c2)) (RangeKey (Index _ coord) dims) = case (width dims) of
  1 -> False -- if width of the dragged col area was 1, then happeneds.
  _ -> or [
      between tlc (tlc + (width dims) - Col 1) $ c1
    , between (tlc + Col 1) (tlc + (width dims) - Col 1) $ c2
    ]
  where tlc = coord^.col

fatCellGotMutated (RowMutate (Drag r1 r2)) (RangeKey (Index _ coord) dims) = case (height dims) of
  1 -> False -- if height of the dragged row area was 1, then nothing happened.
  _ -> or [
      between tlr (tlr + (height dims) - Row 1) $ r1
    , between (tlr + Row 1) (tlr + (height dims) - Row 1) $ r2
    ]
  where tlr = coord^.row

-- Only works for finite ranges for now.
-- #incomplete not actually correct for dragging columns; in sheets, if we drag B to F, and the range was from A1:D4,
-- the new ranges become A1:A4, B1:C4, and F1:F4 (or something like that).
-- #ExposedConstructor : Coord
rangeMutate :: Mutate -> (ASRange -> [ASRange])
rangeMutate mt@(ColMutate (Delete c)) rng@(Range sid ((tlCol, tlRow), (Finite blCol, Finite blRow)))
  | tlCol > blCol            = error "improperly oriented range passed into rangeMutate"
  | tlCol == c && blCol == c = []
  | tlCol == c             = [makeFiniteRange sid (makeCoord tlCol tlRow) (makeCoord (blCol-Col 1) blRow)] -- not (makeCoord tlCol+1 tlRow, C
  | blCol == c             = [makeFiniteRange sid (makeCoord tlCol tlRow) (makeCoord (blCol-Col 1) blRow)]
  | otherwise                 = rangeMutate' mt rng
rangeMutate mt@(RowMutate (Delete r)) rng@(Range sid ((tlCol, tlRow), (Finite blCol, Finite blRow)))
  | tlRow > blRow            = error "improperly oriented range passed into rangeMutate"
  | tlRow == r && blRow == r = []
  | tlRow == r             = [makeFiniteRange sid (makeCoord tlCol tlRow) (makeCoord blCol (blRow-Row 1))]
  | blRow == r             = [makeFiniteRange sid (makeCoord tlCol tlRow) (makeCoord blCol (blRow-Row 1))]
  | otherwise                 = rangeMutate' mt rng
rangeMutate mt rng = rangeMutate' mt rng

-- Assumes none of the boundaries of the range got deleted, in which case the indexMutate's should
-- never be Nothing.
rangeMutate' :: Mutate -> (ASRange -> [ASRange])
rangeMutate' mt (Range sid (c1, extC2)) =  [makeFiniteRange sid c1' c2']
  where
    c1' = fromJust $ coordMutate mt c1
    c2' = fromJust $ coordMutate mt c2
    c2 = fromExtendedCoord $ extC2


condFormattingRulesMutate :: Mutate -> CondFormatRule -> Maybe CondFormatRule
condFormattingRulesMutate mt cfr = cfr'
  where
    cellLocs' = concatMap (rangeMutate mt) (cellLocs cfr)
    formatMapConstructor' = condFormatConditionMutate mt (formatMapConstructor cfr)
    cfr' = if null cellLocs' then Nothing else Just $ cfr { cellLocs = cellLocs', formatMapConstructor = formatMapConstructor' }

condFormatConditionMutate :: Mutate -> FormatMapConstructor -> FormatMapConstructor
condFormatConditionMutate mt (BoolFormatMapConstructor boolCond prop) = BoolFormatMapConstructor (boolCondFormatConditionMutate mt boolCond) prop
condFormatConditionMutate mt (LambdaFormatMapConstructor lExpr) = LambdaFormatMapConstructor lExpr'
  where 
    lExprPython  = Expression lExpr Python
    lExprPython' = expressionMutate mt lExprPython
    lExpr'       = lExprPython'^.expression
    -- #expressiontypeclass would be much better if we had a typeclass which expressionMutate could be applied to

boolCondFormatConditionMutate :: Mutate -> BoolCondition -> BoolCondition
boolCondFormatConditionMutate mt (CustomBoolCond xp) = CustomBoolCond (expressionMutate mt xp)
boolCondFormatConditionMutate mt (NoExprBoolCond typ) = NoExprBoolCond typ
boolCondFormatConditionMutate mt (OneExprBoolCond typ xp) = OneExprBoolCond typ (expressionMutate mt xp)
boolCondFormatConditionMutate mt (TwoExprBoolCond typ xp1 xp2) = TwoExprBoolCond typ (expressionMutate mt xp1) (expressionMutate mt xp2)
