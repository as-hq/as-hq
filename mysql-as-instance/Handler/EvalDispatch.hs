module Handler.EvalDispatch where

import Import
import AS.HandlerLibrary
import qualified AS.DB as DB
import qualified AS.Dispatch as DP

getEvalCellsR :: Handler Value
getEvalCellsR = interactHandlerJson "cells" process
  where process stringCells = do
    let locs = map toLocation stringCells
    evaluatedCells <- liftIO . DP.evalCells $ locs
    liftIO . DB.saveResults $ evaluatedCells
    return . (map cellValue) $ evaluatedCells

putEvalCellR :: Handler Value
putEvalCellR = interactHandlerJson "cell" process
  where process = liftIO . (DP.updateCell <$> cellLocation <*> cellExpression)

getEvalReplR :: Handler Value
getEvalReplR = interactHandlerJson "expression" process
  where process = liftIO . DP.evalRepl
