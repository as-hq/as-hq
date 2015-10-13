{-# LANGUAGE OverloadedStrings #-}

import AS.Types.Core
import AS.Types.DB
import AS.DB.API as DB
import AS.DB.Util as DU
import AS.Util

import AS.Kernels.Python.Eval as KP
import AS.Kernels.LanguageUtils
import AS.Kernels.Excel.Compiler as EC
import AS.Kernels.Excel.Eval as EE

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BU
import Foreign
import Foreign.C.Types
import Foreign.C.String(CString(..))
import Foreign.C

import qualified Data.Text as T 
import qualified Data.List as L
import qualified Data.Map as M

import Database.Redis as R
import Text.ParserCombinators.Parsec

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

testEdges :: Int -> [(ASIndex,ASIndex)]
testEdges n = L.zip (testLocs n) (testLocs n)

main :: IO ()
main = do 
    putStrLn ""
    printWithTime "Running tests..."
    conn <- R.connect DU.cInfo
    printWithTime "hedis database connection: PASSED"
    --testSetCells
    --testLocationKey conn
    --testSheetCreation conn
    --testEvaluate
    --testEvaluateRepl
    --putStrLn $ trimWhitespace Python "  1+1;  \t \n "
    --testIntrospect
    --testExcelExpr
    --testGetCells
    testCopySanitization

testIntrospect :: IO ()
testIntrospect = do
    putStrLn . show =<< (runEitherT $ introspectCode SQL "select * from A1:B4 where a > 1")

testEvaluate :: IO ()
testEvaluate = do
    result <- runEitherT $ KP.evaluate "a=4;b=5\na+b"
    let testResult = (==) result $ Right (ValueD 9.0)
    printWithTime $ "python evaluate: " ++ (showResult testResult)
    (Right (ValueError _ _ _ _)) <- runEitherT $ KP.evaluate "1+a"
    printWithTime $ "python error: PASSED"
    (Left SyntaxError) <- runEitherT $ KP.evaluate "1+"
    printWithTime $ "python syntax error: PASSED"

testEvaluateRepl :: IO ()
testEvaluateRepl = do
    result <- runEitherT $ KP.evaluateRepl "import random"
    printWithTime $ "python repl import: " ++ (showResult $ result == (Right NoValue))
    (Right (ValueD _)) <- runEitherT $ KP.evaluate "random.random()"
    (Right NoValue) <- runEitherT $ KP.evaluateRepl "def myFunc(x):\n\treturn x ** 3"
    printWithTime $ "python repl cell call: PASSED"

testLocationKey :: Connection -> IO ()
testLocationKey conn = do
    DB.setCells $ testCells 1
    let loc = Index "" (1,1)
    let key = DU.getLocationKey loc
    (Right result) <- runRedis conn $ exists key
    printWithTime $ "location key exists: " ++ (showResult result)

testSetCells :: IO () 
testSetCells = do
    let cells = testCells 10
    --printWithTime $ L.concat $ L.map show2 cells
    DB.setCells cells
    let locs = testLocs 10
    cells' <- DB.getCells locs 
    let result = (==) 10 $ length . filterNothing $ cells'
    printWithTime $ "set 100K cells: " ++ (showResult result)

showResult :: Bool -> String
showResult True = "PASSED"
showResult False = "FAILED"

--testSetCellsRaw :: IO () 
--testSetCellsRaw = do
--    let cells = testCells 100000
--    let str = L.intercalate "@" $ (L.map (show2 . cellLocation) cells) ++ (L.map show2 cells)
--    let msg = BU.unsafePackAddressLen (length str) str
--    --printWithTime $ L.concat $ L.map show2 cells
--    _ <- BU.unsafeUseAsCString msg $ \lstr -> do
--       c_setCells lstr (fromIntegral . L.length $ cells)
--    return ()

testGetCells :: IO ()
testGetCells = do
    let locs = testLocs 1000000
    --let cells = testCells 1
    --let keys = map DU.getLocationKey locs
    --DB.setCells cells
    --cells <- DB.getCells locs
    cells <- DB.getCells locs
    printWithTime $ "done"

testSheetCreation :: Connection -> IO ()
testSheetCreation conn = do
    let sid = T.pack "sheetid1"
    let sheet = Sheet sid "sheetname" $ Blacklist [] 
    let wbs = WorkbookSheet "workbookname" [sheet]
    DB.createWorkbookSheet conn wbs
    allWbs <- getAllWorkbookSheets conn
    --printWithTime $ "set 1: " ++ (show allWbs) 
    let sid2 = T.pack "sheetid2"
    let wbs2 = WorkbookSheet "workbookname" [Sheet sid2 "sheetname2" (Blacklist [])]
    DB.createWorkbookSheet conn wbs2
    allWbs' <- getAllWorkbookSheets conn
    --printWithTime $ "set 2: " ++ (show allWbs')
    let result = (length allWbs') > (length allWbs)
    printWithTime $ "new workbooksheet creation: " ++ (showResult result)

testCopySanitization :: IO ()
testCopySanitization = do
    let listCells = testListCells "key" 3
    let cells = testCells 3
    printWithTime . show$ partitionByListKeys (cells ++ listCells) ["key"]
    --printWithTime . show $ map (isMemberOfSpecifiedList "key") listCells 

testListCells :: ListKey -> Int -> [ASCell]
testListCells key size = map (\i -> Cell (Index "" (1,i)) (Expression "" Python) (ValueI 3) [ListMember key]) [1..size]
