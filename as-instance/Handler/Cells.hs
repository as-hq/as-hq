module Handler.Cells where

import Import
import AS.HandlerLibrary
import AS.Types hiding (error)
import qualified AS.DB as DB
import qualified AS.Dispatch as DP

optionsCellsR :: Handler RepPlain
optionsCellsR = do
    addCorsHeaders
    return $ RepPlain $ toContent ("" :: Text)

getCellsR :: Handler Value
getCellsR = interactHandlerJson process
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
putCellsR = interactHandlerJson process
  where process = DP.propagateCell <$> cellLocation <*> cellExpression

postCellsR :: Handler Value
postCellsR = interactHandlerJson process
  where
    process cell = do
      result <- DP.propagateCell (cellLocation cell) (cellExpression cell)
      -- $(logInfo) $ (fromString $ show result)
      return result

-- delete not working with angular -> workaround is post to /clear
deleteCellsR :: Handler Value
deleteCellsR = interactHandlerJson DB.deleteCell

{--
getEvalReplR :: Handler Value
getEvalReplR = interactHandlerJson "expression" process
  where process = liftIO . DP.evalRepl
        --}


-- JSON Syntax: 

-- {
--     "cellLocation": {
--       "tag": "Range",
--       "range": [[2,1],[2,5]]
--     },
--     "cellExpression": {
--       "expression": "A1+1"
--     },
--     "cellValue": {
--       "tag": "ValueS",
--       "contents": "poop"
--     }
-- }