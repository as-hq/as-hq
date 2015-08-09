
module AS.DB where

import AS.Types
import AS.Parsing.Common
import AS.Parsing.Out	
import Prelude

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
