module Handler.Cells where

import Import
import AS.HandlerLibrary
import AS.Types
import qualified AS.DB as DB
import qualified AS.Dispatch as DP

getCellsR :: Handler Value
getCellsR = interactHandlerJson "cells" process
  where
    process :: [ASCell] -> Handler [ASValue]
    process stringCells =
      let locs = map cellLocation stringCells
      in do
        evaluatedCells <- DP.evalCells $ locs
        case evaluatedCells of
          Nothing -> error "Fuck you"
          Just justCells -> return . (map cellValue) $ justCells

putCellsR :: Handler Value
putCellsR = interactHandlerJson "cell" process
  where process = DP.updateCell <$> cellLocation <*> cellExpression

{--
getEvalReplR :: Handler Value
getEvalReplR = interactHandlerJson "expression" process
  where process = liftIO . DP.evalRepl
        --}
