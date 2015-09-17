import AS.Types
import AS.DB.API as DB
import AS.DB.Util as DU

import Data.Text as T
import qualified Data.List as L

import Database.Redis as R

testLocs :: Int -> [ASLocation]
testLocs n = [Index (T.pack "hi") (i,i) | i <-[0..n]]

testEdges :: Int -> [(ASLocation,ASLocation)]
testEdges n = L.zip (testLocs n) (testLocs n)

main :: IO ()
main = do 
    putStrLn $ "Starting test"
    conn <- R.connect DU.cInfo
    testSheetCreation conn

testSheetCreation :: R.Connection -> IO ()
testSheetCreation conn = do
    let sid = T.pack "sheetid1"
    let sheet = Sheet sid "sheetname" $ Blacklist []
    let wbs = WorkbookSheet "workbookname" [sheet]
    DB.createWorkbookSheet conn wbs
    allWbs <- getAllWorkbooksheets conn
    putStrLn $ "set 1: " ++ (show allWbs)
    let sid2 = T.pack "sheetid2"
    let wbs2 = WorkbookSheet "workbookname" [Sheet sid2 "sheetname2" (Blacklist [])]
    DB.createWorkbookSheet conn wbs2
    allWbs' <- getAllWorkbookSheets conn
    putStrLn $ "set 2: " ++ (show allWbs')