module AS.Handlers.Mutate (handleMutateSheet) where

import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User
import AS.Types.Excel
import AS.DB.Util as DU
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
  allCells <- DB.getCellsInSheet (userSheetId uc)
  conn <- dbConn <$> readMVar state
  let newCells = map (cellMap mutateType) allCells
      oldCellsNewCells = zip allCells newCells
      oldCellsNewCells' = filter (\(c, c') -> (isNothing c') || (c /= fromJust c')) oldCellsNewCells
      -- ^ get rid of cells that haven't changed.
      oldCells' = map fst oldCellsNewCells
      blankedCells = blankCellsAt (map cellLocation oldCells')
      newCells' = catMaybes $ map snd oldCellsNewCells
      updatedCells   = mergeCells newCells' blankedCells -- eval blanks at the old cell locations, re-eval at new locs
  updateMsg <- runDispatchCycle state updatedCells (userCommitSource uc)
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
cellMap mt c@(Cell loc xp v ts) = case ((cellLocMap mt) loc) of 
  Nothing -> Nothing 
  Just loc' -> let c' = Cell loc' ((expressionMap mt) xp) v ts 
    in case xp of 
      Expression _ _ -> Just c'
      Coupled _ _ _ _ -> if DU.isFatCellHead c
        then Just $ DU.toUncoupled c' 
        else Just $ DU.decoupleCell c' 