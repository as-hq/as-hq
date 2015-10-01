{-# LANGUAGE OverloadedStrings #-}

import AS.Types.Core
import AS.Types.DB
import AS.DB.API as DB
import AS.DB.Util as DU
import AS.Util

import AS.Kernels.Python.Eval as KP
import AS.Kernels.LanguageUtils
import AS.Kernels.Excel.Compiler as E

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BU
import Foreign
import Foreign.C.Types
import Foreign.C.String(CString(..))
import Foreign.C

import Data.Text as T hiding (index, length)
import qualified Data.List as L

import Database.Redis as R
import Text.ParserCombinators.Parsec

testEdges :: Int -> [(ASLocation,ASLocation)]
testEdges n = L.zip (testLocs n) (testLocs n)

main :: IO ()
main = do 
    putStrLn ""
    printTimed "Running tests..."
    conn <- R.connect DU.cInfo
    printTimed "hedis database connection: PASSED"

    --testSetCells
    --testLocationKey conn
    --testSheetCreation conn
    --testEvaluate
    --testEvaluateRepl
    --testIntrospect
    --testExcelExpr

testExcelExpr :: IO ()
testExcelExpr = do
    let str = "=f(B2:index(A2:C6,5,2))"
    let val = parse E.formula "" str
    putStrLn $ "parsed xp: " ++ (show val)

testIntrospect :: IO ()
testIntrospect = do
    putStrLn . show =<< introspectCodeRepl Python "import random"

testEvaluate :: IO ()
testEvaluate = do
    result <- KP.evaluate "a=4;b=5\na+b"
    let testResult = (==) result $ Right (ValueD 9.0)
    printTimed $ "python evaluate: " ++ (showResult testResult)
    (Right (ValueError _ _ _ _)) <- KP.evaluate "1+a"
    printTimed $ "python error: PASSED"
    (Left SyntaxError) <- KP.evaluate "1+"
    printTimed $ "python syntax error: PASSED"

testEvaluateRepl :: IO ()
testEvaluateRepl = do
    result <- KP.evaluateRepl "import random"
    printTimed $ "python repl import: " ++ (showResult $ result == (Right NoValue))
    (Right (ValueD _)) <- KP.evaluate "random.random()"
    printTimed $ "python repl cell call: PASSED"

testLocationKey :: Connection -> IO ()
testLocationKey conn = do
    DB.setCells $ testCells 1
    let loc = Index "" (1,1)
    let key = DU.getLocationKey loc
    (Right result) <- runRedis conn $ exists key
    printTimed $ "location key exists: " ++ (showResult result)

testSetCells :: IO () 
testSetCells = do
    let cells = testCells 100000
    --printTimed $ L.concat $ L.map show2 cells
    DB.setCells cells
    cells' <- testGetCells 
    let result = (==) 100000 $ length . filterNothing $ cells'
    printTimed $ "set 100K cells: " ++ (showResult result)

showResult :: Bool -> String
showResult True = "PASSED"
showResult False = "FAILED"

--testSetCellsRaw :: IO () 
--testSetCellsRaw = do
--    let cells = testCells 100000
--    let str = L.intercalate "@" $ (L.map (show2 . cellLocation) cells) ++ (L.map show2 cells)
--    let msg = BU.unsafePackAddressLen (length str) str
--    --printTimed $ L.concat $ L.map show2 cells
--    _ <- BU.unsafeUseAsCString msg $ \lstr -> do
--       c_setCells lstr (fromIntegral . L.length $ cells)
--    return ()

testGetCells :: IO [Maybe ASCell] 
testGetCells = do
    let locs = testLocs 100000
    DB.getCells locs

testSheetCreation :: Connection -> IO ()
testSheetCreation conn = do
    let sid = T.pack "sheetid1"
    let sheet = Sheet sid "sheetname" $ Blacklist []
    let wbs = WorkbookSheet "workbookname" [sheet]
    DB.createWorkbookSheet conn wbs
    allWbs <- getAllWorkbookSheets conn
    --printTimed $ "set 1: " ++ (show allWbs)
    let sid2 = T.pack "sheetid2"
    let wbs2 = WorkbookSheet "workbookname" [Sheet sid2 "sheetname2" (Blacklist [])]
    DB.createWorkbookSheet conn wbs2
    allWbs' <- getAllWorkbookSheets conn
    --printTimed $ "set 2: " ++ (show allWbs')
    let result = (length allWbs') > (length allWbs)
    printTimed $ "new workbooksheet creation: " ++ (showResult result)