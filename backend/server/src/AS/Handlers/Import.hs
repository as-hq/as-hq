module AS.Handlers.Import where

import AS.Types.Cell
import AS.Types.CellProps
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User
import AS.Types.Eval
import AS.Types.Commits
import Control.Concurrent
import Data.Either

import AS.Reply

import qualified Data.Csv as CSV
import qualified Data.Vector as V
import qualified AS.Parsing.Read as PR
import qualified Data.Maybe as DM
import qualified Data.ByteString.Lazy as BL
import qualified AS.DB.Transaction as DT 

import Control.Lens hiding ((.=))

-- used for importing arbitrary files 
-- handleImport :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
-- handleImport uc state msg = return () -- TODO

-- Simply update the DB with the CSV data, and do a "trivial" parsing eval. No propagation/dispatch for an initial import.
-- Frontend already put the file in the static folder, and all cells created will have the default language passed in
handleCSVImport :: ASUserClient -> MVar ServerState -> ASIndex -> ASLanguage -> String -> IO ()
handleCSVImport uc mstate ind lang fileName = do 
  csvData <- BL.readFile $ "static/" ++ fileName
  state <- readMVar mstate
  let src = userCommitSource uc
  let decoded = CSV.decode CSV.NoHeader csvData :: Either String (V.Vector (V.Vector String))
  case decoded of 
    Left e -> putStrLn e >> return ()
    Right csv -> do 
      -- Create cells, taking offset, lang, and parsing into account
      let indices = imap2D (\dx dy -> DM.fromJust $ shiftInd (Offset dx dy) ind) csv
          values = map2D (csvValue lang) csv
          vCells = zipWith3In2D (\ind str val -> Cell ind (Expression str lang) val emptyProps Nothing Nothing) indices csv values
          cells = toList2D vCells
      -- generate and push commit to DB
      commit <- generateCommitFromCells cells
      DT.updateDBWithCommit (state^.appSettings.graphDbAddress) (state^.dbConn) src commit
      -- send list of cells back to frontend
      broadcastSheetUpdate mstate $ sheetUpdateFromCells cells

-- Map a function over a 2D vector
map2D :: (a -> b) -> V.Vector (V.Vector a) -> V.Vector (V.Vector b)
map2D f = V.map (V.map f)

-- Map a function over a 2D vector with index. First arg is dX, second is dY.
imap2D :: (Col -> Row -> b) -> V.Vector (V.Vector a) -> V.Vector (V.Vector b)
imap2D f csv = V.imap innerMap csv
  where
    innerMap i = V.imap (\j _ -> f j i)

-- ZipWith3 in 2D, used to construct cells
zipWith3In2D :: (a -> b -> c -> d) -> V.Vector (V.Vector a) -> V.Vector (V.Vector b) -> V.Vector (V.Vector c) -> V.Vector (V.Vector d)
zipWith3In2D f = V.zipWith3 (V.zipWith3 f)

-- Convert 2D vector to list
toList2D :: V.Vector (V.Vector a) -> [a]
toList2D = V.toList . V.concat . V.toList 

-- Given a language and CSV data, produce an ASValue by trivial parsing (no eval)
-- If parsing ever returns an expanding cell, return a ValueError. If parsing normally didn't work, return a ValueS.
csvValue :: ASLanguage -> String -> ASValue
csvValue lang s = case PR.parseValue lang s of
  Left e -> ValueS s
  Right (Expanding _) -> ValueError "Couldn't parse value" "CSVParse"
  Right (CellValue v) -> v
