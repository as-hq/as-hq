module AS.Handlers.Import where

import AS.Prelude
import AS.Logging

import AS.Config.Constants (import_message_id)
import AS.DB.Clear as DC
import AS.DB.Graph as G
import AS.DB.API as DB
import AS.Types.Cell
import AS.Types.CellProps as CP
import AS.Types.Network
import AS.Types.Messages
import AS.Types.Eval
import AS.Types.Shift
import AS.Types.Commits
import AS.Types.DB hiding (Clear)
import AS.Types.Graph
import Control.Concurrent
import Data.Either

--  Used to custom parse json obtained from importing an xls file.
import AS.Handlers.Misc (handleClear)
import AS.Handlers.Sheets (handleOpenSheet)
import AS.Parsing.Read
import AS.Parsing.Common as C
import AS.Types.Graph (read2)
import AS.Types.RangeDescriptor (JSON)
import Data.Maybe (mapMaybe)
import qualified AS.Kernels.Python.Client as KP
import qualified AS.Kernels.Python.Types as KT
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8 as BC
-- end custom parse

import AS.Reply

import AS.Parsing.Show (showValue)
import qualified AS.Parsing.Read          as PR
import qualified AS.Util                  as U
import qualified AS.Serialize             as S
import qualified AS.DB.Transaction        as DT 
import qualified AS.DB.Export             as DX

import Data.Either
import qualified Data.Csv as CSV
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import qualified Network.WebSockets as WS

import qualified AS.DB.Transaction as DT 

import Control.Monad (void)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class (lift)

-- #anand used for importing binary alphasheets files (making a separate REST server for alphasheets
-- import/export seems overkill given that it's a temporarily needed solution)
-- so we just send alphasheets files as binary data over websockets and immediately load
-- into the current sheet.
handleImportBinary :: (Client c) => c -> State -> BL.ByteString -> IO ()
handleImportBinary c mstate bin = $undefined

handleExport :: MessageContext -> SheetID -> IO ()
handleExport msgctx exportSid = do
  exported <- DX.exportSheetData (msgctx^.dbConnection) exportSid
  WS.sendBinaryData (msgctx^.userClient.userConn) (S.encodeLazy exported)

handleExportCell :: MessageContext -> ASIndex -> IO ()
handleExportCell msgctx idx = do
  cell <- DB.getCell (msgctx^.dbConnection) idx
  whenJust cell $ \c -> 
    let exp = showValue (c^.cellExpression.language) (CellValue $ c^.cellValue)
    in sendAction msgctx $ ExportCellData idx exp

-- #RoomForImprovement: Timchu. Right now, any error in EvaluateRequest, or in
-- pattern matching that that to an EvaluateReply, or in Parsing, or in
-- extracting Excel Cells from the jsonBlob, will give an undescriptive
-- Left parseError. That is the extent of error handling in evaluateExcelSheet.
-- Timchu, 2/15/16.
evaluateExcelSheet :: MessageId -> SheetID -> EvalCode -> EitherTExec [ASCell]
evaluateExcelSheet mid sid code = do
  (KT.EvaluateReply val err disp) <- KP.runRequest $ KT.EvaluateRequest KT.Cell mid sid code
  let maybeCells = do
                jsonString <- val 
                jsonBlob <- case parseOnly (json Python) (BC.pack jsonString) of
                              Left _ ->  Nothing
                              Right v -> Just v
                extractExcelCells sid jsonBlob
  maybe (left ParseError) return maybeCells

excelImportFuncString :: EvalCode
excelImportFuncString = "readSheet"

-- Note: timchu. I don't really like how Handlers have more than just broadcast
-- and one function. This makes me not able to reuse the functionality of
-- HandleClear without broadcasting the result.
handleExcelImport :: MessageContext -> SheetID -> String -> IO ()
handleExcelImport msgctx sid fileName = do
  let code = excelImportFuncString ++ "('" ++ fileName ++ "')"
      conn = msgctx^.dbConnection
      uid = msgctx^.userClient.userId
      mid = msgctx^.messageId
  update <- runEitherT $ do
              cells <- evaluateExcelSheet mid sid code
              -- clears the sheet, sends a message to the frontend.
              lift $ handleClear msgctx sid
              lift $ DB.setCells conn cells
              G.setCellsAncestors conn uid cells
              return $ sheetUpdateFromCells cells
  broadcastErrOrUpdate msgctx update

-- used for importing arbitrary files 
-- handleImport :: UserClient -> State -> ASPayload -> IO ()
-- handleImport uc state msg = return () -- TODO

-- Simply update the DB with the CSV data, and do a "trivial" parsing eval. No propagation/dispatch for an initial import.
-- Frontend already put the file in the static folder, and all cells created will have the default language passed in
  

handleCSVImport :: MessageContext -> ASIndex -> ASLanguage -> String -> IO ()
handleCSVImport msgctx ind lang fileName = do 
  csvData <- BL.readFile $ "static/" ++ fileName
  let src = messageCommitSource msgctx
  let decoded = CSV.decode CSV.NoHeader csvData :: Either String (V.Vector (V.Vector String))
  case decoded of 
    Left e -> void (putsError src $ "Could not decode CSV: " ++ e)
    Right csv -> do 
      -- Create cells, taking offset, lang, and parsing into account
      let indices = imap2D (\dx dy -> $fromJust $ shiftByOffsetWithBoundsCheck (Offset dx dy) ind) csv
          values = map2D (csvValue lang) csv
          vCells = zipWith3In2D (\ind str val -> Cell ind (Expression str lang) val emptyProps Nothing Nothing) indices csv values
          cells = toList2D vCells
      -- generate and push commit to DB
      commit <- generateCommitFromCells cells
      DT.updateDBWithCommit (msgctx^.dbConnection) src commit
      -- send list of cells back to frontend
      broadcastSheetUpdate msgctx $ sheetUpdateFromCells cells

-- Map a function over a 2D vector
map2D :: (a -> b) -> V.Vector (V.Vector a) -> V.Vector (V.Vector b)
map2D f = V.map (V.map f)

-- Map a function over a 2D vector with index. First arg is dX, second is dY.
imap2D :: (Col -> Row -> b) -> V.Vector (V.Vector a) -> V.Vector (V.Vector b)
imap2D f = V.imap innerMap
  where
    innerMap i = V.imap (\j _ -> f (Col j) (Row i))

-- ZipWith3 in 2D, used to construct cells
zipWith3In2D :: (a -> b -> c -> d) -> V.Vector (V.Vector a) -> V.Vector (V.Vector b) -> V.Vector (V.Vector c) -> V.Vector (V.Vector d)
zipWith3In2D f = V.zipWith3 (V.zipWith3 f)

-- Convert 2D vector to list
toList2D :: V.Vector (V.Vector a) -> [a]
toList2D = V.toList . V.concat . V.toList 

-- Given a language and CSV data, produce an ASValue by trivial parsing (no eval)
-- If parsing ever returns an expanding cell, return a ValueError. If parsing normally didn't work, return a ValueS.
csvValue :: ASLanguage -> String -> ASValue
csvValue lang s = case PR.parseValue lang (BC.pack s) of
  Left e -> ValueS s
  Right (Expanding _) -> ValueError "Couldn't parse value" "CSVParse"
  Right (CellValue v) -> v


-- *********************************************
-- Helper methods for converting Strings to vals, indices, and expressions.
-- Used in handleImportExcel, when parsing python-serialized cells from
-- excel sheets.
-- There is no type safety since we're playing around with strings.
-- These functions should be entirely hidden from the user.

-- *******

-- input: SheetID, string of form "(1,4)"
-- output: ASIndex sid (Coord 1 4)
stringToInd :: SheetID -> String -> ASIndex
stringToInd sid s = Index sid (read2 s :: Coord)

stringToVal :: String -> ASValue
stringToVal s = $fromRight $ parseOnly (asValue Excel) (BC.pack s)

--input: string of form "=A1+4")
stringToXp :: String -> ASExpression
stringToXp s = Expression s Excel

specToCell :: ASIndex -> ASExpression -> ASValue -> ASCell
specToCell ind xp val = Cell ind xp val CP.emptyProps Nothing Nothing

-- Turns a JSON dump of the form
-- >> "{'formula': '=A1+1', 'location': '(1, 1)', 'value': '1'}"
-- into an ASCell. Used in extractExcelCells.
-- TODO(tim): There is very little type safety in the current implementation.
-- That means this function should be entirely obscured from the user.
cellJsToCell :: SheetID -> JSON -> Maybe ASCell
cellJsToCell sid js = do
  let coordKey = "location"
      xpKey    = "formula"
      valKey   = "value"
  let f (JSONLeaf (SimpleValue (ValueS s))) = s
  [coordS, xpS, valS] <- mapM (fmap f . (js .>)) [coordKey, xpKey, valKey]
  let ind = stringToInd sid coordS
      xp  = stringToXp  xpS
      val = stringToVal valS
  return $ specToCell ind xp val

-- Turns a JSON dump of the form
-- >>  "{'cells': [{'formula': '=A1+1', 'location': '(1, 1)', 'value': '1'}]}"
-- into Maybe ASCells, where the result is Nothing iff extractNestedListItems
-- fails.
extractExcelCells :: SheetID -> JSON -> Maybe [ASCell]
extractExcelCells sid js = do
  let key = "cells"
  cellJsList <- extractNestedListItems js key
  -- If any of the cellJs's don't parse correctly, that cell will simply 
  -- not show up in the imports. The function only outputs
  -- nothing if extractNestedListItems fails. Timchu, 2/15/16.
  return $ mapMaybe (cellJsToCell sid) cellJsList
