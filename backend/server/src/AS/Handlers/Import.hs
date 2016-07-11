{-# LANGUAGE OverloadedStrings #-}

module AS.Handlers.Import where

import System.Directory
import System.IO
import System.Posix.IO
import System.Posix.Files
import Control.Monad (void)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class (lift)
import Control.Concurrent
import Data.Either
import Data.Maybe (mapMaybe)
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Csv as CSV
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Network.WebSockets as WS

import AS.Prelude
import AS.Logging
import AS.Config.Constants (import_message_id)
import AS.Config.Settings
import AS.Types.Cell
import AS.Types.CellProps as CP
import AS.Types.Network
import AS.Types.Messages
import AS.Types.Eval
import AS.Types.Shift
import AS.Types.Commits
import AS.Types.RangeDescriptor (JSON)
import AS.Types.DB hiding (Clear)
import AS.Types.Graph

import AS.DB.Clear as DC
import AS.DB.Graph as G
import AS.DB.API as DB
import AS.Handlers.Sheets (handleAcquireWorkbook, handleAcquireSheet)
import AS.Handlers.Misc (handleClear)
import AS.Handlers.Sheets (handleOpenSheet)
import AS.Parsing.Read
import AS.Parsing.Show (showValue)
import AS.Parsing.Common as C
import AS.Reply
import qualified AS.Kernels.Python.Client as KP
import qualified AS.Kernels.Python.Types as KT
import qualified AS.Util                  as U
import qualified AS.Serialize             as S
import qualified AS.DB.Transaction        as DT 
import qualified AS.DB.Export             as DX


-- #anand used for importing binary alphasheets files (making a separate REST server for alphasheets
-- import/export seems overkill given that it's a temporarily needed solution)
-- so we just send alphasheets files as binary data over websockets and immediately load
-- into the current sheet.
handleImportBinaryUser :: MessageContext -> BL.ByteString -> IO ()
handleImportBinaryUser msgctx bin = do
  let conn  = msgctx^.dbConnection
      uid   = messageUserId msgctx
  case (S.decodeLazy bin :: Either String SheetExportData) of
    Left s -> do
      case (S.decodeLazy bin :: Either String WorkbookExportData) of 
        Left s -> do
          putStrLn $ "import error: \n\n" ++ s
          sendAction msgctx $ ShowFailureMessage $ "could not process binary file, decode error: " ++ s
        Right ex -> 
          DX.importWorkbookData conn uid ex >>=
          handleAcquireWorkbook msgctx 
    Right ex -> 
      DX.importSheetData conn uid ex >>=
      handleAcquireSheet msgctx 

handleExportWorkbook :: MessageContext -> WorkbookID -> IO ()
handleExportWorkbook msgctx wid = do
  exported <- DX.exportWorkbookData (msgctx^.dbConnection) wid
  WS.sendBinaryData (msgctx^.userClient.userConn) (S.encodeLazy exported)

handleExportCell :: MessageContext -> ASIndex -> IO ()
handleExportCell msgctx idx = do
  cell <- DB.getCell (msgctx^.dbConnection) idx
  whenJust cell $ \c -> 
    let exp = showValue (c^.cellExpression.language) (CellValue $ c^.cellValue)
    in sendAction msgctx $ ExportCellData idx exp

--------------------------------------------------------------------------------
-- Checkpoints

-- TODO(riteshr) This is a somewhat hacky implementation that saves to files
-- instead of the DB, where descriptions are at the top of the file, to avoid 
-- migrations and because this probably won't be
-- a significantly used feature. #needsrefactor

-- | When the user asks to make a checkpoint, save exported data to a file
-- in checkpoint/wid/uid, with the timestamp as name.
handleMakeCheckpoint :: MessageContext -> String -> IO ()
handleMakeCheckpoint msgctx desc = do 
  let wid = messageWorkbookId msgctx
  exported <- DX.exportWorkbookData (msgctx^.dbConnection) wid
  fileName <- getTime
  saveToFile msgctx exported fileName desc

-- | Get all the file names of the checkpoints for a given user/workbook.
handleGetAllCheckpoints :: MessageContext -> IO ()
handleGetAllCheckpoints msgctx = do 
  let uid = msgctx^.userClient.userId
  let wid = messageWorkbookId msgctx
  print wid
  let wbDir = intercalate "/" [checkpoint_dir, T.unpack wid]
  users <- listDirectory wbDir
  checkpoints <- concat <$> (forM users $ \user -> do 
    files <- listDirectory $ checkpointDirForUser msgctx user
    let filePaths = map (\n -> checkpointPathForUser msgctx n user) files
    descs <- forM filePaths getDescriptionInFile 
    return $ zipWith3 Checkpoint (repeat user) descs files)
  print checkpoints
  sendAction msgctx $ AllCheckpoints checkpoints

-- | When a user wants to view a checkpoint, we create a temporary checkpoint
-- of the current workbook if it doesn't already exist and then just call
-- import code (which modifies the DB).
handleViewCheckpoint :: MessageContext -> String -> String -> IO ()
handleViewCheckpoint msgctx fileName uid = do 
  makeTempCheckpoint msgctx
  let checkName = checkpointPathForUser msgctx fileName uid
  importCheckpoint msgctx checkName

-- | When applying a checkpoint, we just delete the temporary export and call
-- view code, which modifies the DB.
handleApplyCheckpoint :: MessageContext -> String -> String -> IO ()
handleApplyCheckpoint msgctx fileName uid = do 
  removeTempFile msgctx
  handleViewCheckpoint msgctx fileName uid

-- | If no checkpoint is to be applied, we restore the temporary checkpoint.
handleRevertCheckpoint :: MessageContext -> IO ()
handleRevertCheckpoint msgctx = do 
  let tempFile = checkpointPath msgctx "temp"
  importCheckpoint msgctx tempFile
  removeTempFile msgctx

-- | If a temp checkpoint file doesn't exist, create it
makeTempCheckpoint :: MessageContext -> IO ()
makeTempCheckpoint msgctx = do 
  let tempFile = checkpointPath msgctx "temp"
  let wid = messageWorkbookId msgctx
  exists <- doesFileExist tempFile
  unless exists $ do 
    exported <- DX.exportWorkbookData (msgctx^.dbConnection) wid
    saveToFile msgctx exported "temp" "temp checkpoint"

saveToFile :: MessageContext -> WorkbookExportData -> String -> String -> IO ()
saveToFile msgctx wbData fileName desc = do 
  let filePath = checkpointPath msgctx fileName
  createDirectoryIfMissing True (checkpointDir msgctx)
  createFile filePath stdFileMode
  let txt = BL.concat [BLC.pack desc, "\n", S.encodeLazy wbData]
  BL.writeFile filePath txt

getDescriptionInFile :: String -> IO String
getDescriptionInFile fileName = withFile fileName ReadMode $ hGetLine

importCheckpoint ::  MessageContext -> String -> IO ()
importCheckpoint msgctx fileName = do 
  let wid  = messageWorkbookId msgctx
  let uid  = msgctx^.userClient.userId
  let conn = msgctx^.dbConnection
  contents <- BL.readFile fileName
  -- Filter out description
  let bin = BL.drop 1 $ BLC.dropWhile (/= '\n') contents
  case (S.decodeLazy bin :: Either String WorkbookExportData) of 
    Left s -> do
      putStrLn $ "import error: \n\n" ++ s
      sendAction msgctx $ ShowFailureMessage $ "Checkpoint import failure."
    Right ex -> do
      DX.importWorkbookDataWithID conn uid ex wid
      handleAcquireWorkbook msgctx wid

removeTempFile ::  MessageContext -> IO ()
removeTempFile msgctx = do 
  let tempFile = checkpointPath msgctx "temp"
  removeFile tempFile

-- TODO(riteshr) listDirectory isn't exported in System.Directory?
listDirectory :: String -> IO [String]
listDirectory name = do 
  contents <- getDirectoryContents name
  return $ filter (\n -> n /= "." && n /= ".." && n /= "temp") contents

checkpointPath :: MessageContext -> String -> String
checkpointPath msgctx name = intercalate "/" lst
  where lst = [checkpoint_dir, T.unpack wid, T.unpack uid, name]
        uid = msgctx^.userClient.userId
        wid = messageWorkbookId msgctx

checkpointPathForUser :: MessageContext -> String -> String -> String
checkpointPathForUser msgctx name user = intercalate "/" lst
  where lst = [checkpoint_dir, T.unpack wid, user, name]
        wid = messageWorkbookId msgctx

checkpointDir :: MessageContext -> String
checkpointDir msgctx = (intercalate "/" lst) ++ "/"
  where lst = [checkpoint_dir, T.unpack wid, T.unpack uid]
        uid = msgctx^.userClient.userId
        wid = messageWorkbookId msgctx

checkpointDirForUser :: MessageContext -> String -> String
checkpointDirForUser msgctx user = (intercalate "/" lst) ++ "/"
  where lst = [checkpoint_dir, T.unpack wid, user]
        wid = messageWorkbookId msgctx

--------------------------------------------------------------------------------

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
      let indices = imap2D (\dx dy -> shiftSafe (Offset dx dy) ind) csv
          values = map2D (csvValue lang) csv
          vCells = zipWith3In2D (\(Just ind) str val -> Cell ind (Expression str lang) val emptyProps Nothing Nothing) indices csv values
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
csvValue lang s = case parseValue lang (BC.pack s) of
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
stringToVal s = fromRight $ parseOnly (asValue Excel) (BC.pack s)

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
