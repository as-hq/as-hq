
module AS.DB where

import AS.Types
import AS.Parsing.Common
import AS.Parsing.Out	
import Data.Maybe (isNothing)
import Prelude

------------------------------ handlers ---------------------------------------

handleGet :: ASPayload -> IO ASMessage
handleGet (PayloadLL locs) = do
    cells <- getCells locs
    if any isNothing cells
        then return failureMessage
        else return $ Message NoAction Success (PayloadCL (map (\(Just x)->x) cells))

handleDelete :: ASPayload -> IO ASMessage 
handleDelete (PayloadL loc) = deleteCell loc >> return successMessage
handleDelete (PayloadLL locs) = deleteCells locs >> return successMessage

-----------------------------------------------------------------------------------------------------------------------------------
-- Get and set cell methods (batched)

getCell :: ASLocation -> IO (Maybe ASCell)
getCell loc = return Nothing


getCells :: [ASLocation] -> IO [Maybe ASCell]
getCells locs = return []

setCell :: ASCell -> IO ()
setCell cell = return ()

setCells :: [ASCell] -> IO ()
setCells cells = return ()

deleteCell :: ASLocation -> IO ()
deleteCell loc = return ()

deleteCells :: [ASLocation] -> IO ()
deleteCells locs = return ()

-----------------------------------------------------------------------------------------------------------------------------------
-- DAG operations

dbGetDAG :: IO [(ASLocation, ASLocation)]
dbGetDAG = return []

dbUpdateLocationDepsBatch :: [(ASLocation,[ASLocation])] -> IO ()
dbUpdateLocationDepsBatch depList = return ()

----------------------------------------- Library functions ---------------------------------------------------------------------------------------

------------ DB Boilerplate & models ----------------------------------------

-----------------------------------------------------------------------------------------------------------------------------------
-- Conversion to and from AS Types and DB types
