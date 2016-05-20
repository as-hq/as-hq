module AS.Handlers.Props (handleToggleProp, handleSetProp, handleChangeDecimalPrecision) where

import AS.Prelude
import AS.Config.Constants

import AS.Types.Cell
import AS.Types.Commits
import AS.Types.Eval
import AS.Types.Formats
import AS.Types.Messages
import AS.Types.Network
import AS.Types.User (UserID)
import AS.Types.Updates

import AS.Prelude 
import AS.DB.API
import qualified AS.DB.Transaction as DT
import AS.Dispatch.Core as DP
import AS.Daemon as DM
import AS.Reply
import AS.Util

import Data.List
import qualified Data.Set as S
import qualified Control.Lens as Lens
import Control.Applicative ((<*>))
import Control.Concurrent
import qualified Data.Map as M
import Database.Redis (Connection)

-- | Used only for flag props. 
handleToggleProp :: MessageContext -> CellProp -> ASRange -> IO ()
handleToggleProp msgctx prop rng = do
  let conn = msgctx^.dbConnection
      locs = finiteRangeToIndices rng
      pt = propType prop
  cells <- getPossiblyBlankCells conn locs
  let (cellsWithProp, cellsWithoutProp) = partition (hasPropType pt . view cellProps) cells
  -- if there's a single prop present in the range, remove this prop from all the cells; 
  -- otherwise set the prop in all the cells. 
  let cellToNewProps :: ASCell -> ASCellProps
      cellToNewProps = if (null cellsWithoutProp)
                     then removeProp pt . view cellProps
                     else \c -> if elem c cellsWithoutProp
                               then setProp prop $ c^.cellProps
                               else c^.cellProps
  update <- transformPropsInDatabase msgctx cellToNewProps rng
  broadcastErrOrUpdate msgctx $ Right update
-- don't HAVE to send back the entire cells, but that's an optimization for a later time. 
-- Said toad. (Alex 11/7)

setPropEndware :: ServerState -> CellProp -> ASCell -> IO ()
setPropEndware state (StreamInfo s) c = modifyDaemon state s (c^.cellLocation) evalMsg
  where evalMsg = ServerMessage daemon_message_id $ Evaluate [EvalInstruction (c^.cellExpression) (c^.cellLocation)]
setPropEndware _ _ _ = return ()

removePropEndware :: ServerState -> CellProp -> ASCell -> IO ()
removePropEndware state (StreamInfo s) c = removeDaemon (c^.cellLocation) state
removePropEndware _ _ _ = return ()

-- Given a prop transform, create the new cells by mapping over the transform 
-- We do not "propagate" any of these formats/props to descendants, for now, 
-- even though Google Sheets does this for percents in Excel mode (Ritesh 5/1)
-- We used to do a dispatch of proper descendants, and then patch the roots
-- into the update. 
-- The (ASCell -> ASCellProps) argument of transformPropsInDatabase is a function
-- that takes a cell, and isolates the new cell props to apply to that cell.
transformPropsInDatabase :: MessageContext 
                         -> (ASCell -> ASCellProps) 
                         -> ASRange 
                         -> IO SheetUpdate
transformPropsInDatabase msgctx f rng = do
  let locs = finiteRangeToIndices rng
      conn = msgctx^.dbConnection
      src  = messageCommitSource msgctx
  cs <- getPossiblyBlankCells conn locs
  -- Create new cells by changing props in accordance with cellPropsTransforms 
  -- Add them to an empty evalContext, and update the DB in the same way that 
  -- we do after an eval (add commit + cells to the database, so that this can
  -- be undone. Return the update to frontend.)
  let cells' = map (f >>= (Lens.set cellProps)) cs
  let evalctx = addCellsToContext cells' emptyContext
  runEitherT $ DT.updateDBWithContext conn src evalctx
  return $ evalctx^.updateAfterEval

-- #Lenses.
insertCellsIntoSheetUpdate :: [ASCell] -> SheetUpdate -> SheetUpdate
insertCellsIntoSheetUpdate cells su = su & cellUpdates %~ (insertCellsIntoUpdate cells)

handleSetProp :: MessageContext -> CellProp -> ASRange -> IO ()
handleSetProp msgctx prop rng = 
  transformPropsInDatabase msgctx (setProp prop . view cellProps) rng >>= 
    broadcastErrOrUpdate msgctx . Right

-- Change the decimal precision of all the values in a range
handleChangeDecimalPrecision :: MessageContext -> Int ->  ASRange -> IO ()
handleChangeDecimalPrecision msgctx i rng = 
  transformPropsInDatabase msgctx cellPropsUpdate rng >>= 
    broadcastErrOrUpdate msgctx . Right
  where
    getUpdatedFormat :: ASCell -> Maybe Format
    getUpdatedFormat = view format . shiftDecPrecision i . getFormattedVal
    cellPropsUpdate :: ASCell -> ASCellProps
    cellPropsUpdate  = (maybe id (setProp . ValueFormat) . getUpdatedFormat) <*> (view cellProps)
