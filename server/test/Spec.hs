{-# LANGUAGE OverloadedStrings #-}

import AS.Types.Core
import AS.Types.DB
import AS.DB.API as DB
import AS.DB.Util as DU
import AS.Util

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BU
import Foreign
import Foreign.C.Types
import Foreign.C.String(CString(..))
import Foreign.C

import Data.Text as T hiding (index)
import qualified Data.List as L

import Database.Redis as R

testEdges :: Int -> [(ASLocation,ASLocation)]
testEdges n = L.zip (testLocs n) (testLocs n)

main :: IO ()
main = do 
    printTimed "Starting test"
    conn <- R.connect DU.cInfo
    printTimed "got connection"
    --testSheetCreation conn
    testSetCellsRaw
    printTimed "cells set"
    --cells <- testGetCells
    --printTimed $ "got cells: " ++ (show cells)
    --let loc = Index "" (1,1)
    --DB.setCells $ [Cell loc (Expression "1" Python) (ValueD 1.0) []]
    --cell <- DB.getCells [loc]
    --putStrLn $ "got cell" ++ (show cell)


testSetCells :: IO () 
testSetCells = do
    let cells = testCells 100000
    --printTimed $ L.concat $ L.map show2 cells
    DB.setCells cells

testSetCellsRaw :: IO () 
testSetCellsRaw = do
    let cells = testCells 100000
    let str = L.intercalate "@" $ (L.map (show2 . cellLocation) cells) ++ (L.map show2 cells)
    let msg = BU.unsafePackAddressLen (length str) str
    --printTimed $ L.concat $ L.map show2 cells
    _ <- BU.unsafeUseAsCString msg $ \lstr -> do
       c_setCells lstr (fromIntegral . L.length $ cells)
    return ()

testGetCells :: IO [Maybe ASCell] 
testGetCells = do
    let locs = testLocs 11
    DB.getCells locs

testSheetCreation :: R.Connection -> IO ()
testSheetCreation conn = do
    let sid = T.pack "sheetid1"
    let sheet = Sheet sid "sheetname" $ Blacklist []
    let wbs = WorkbookSheet "workbookname" [sheet]
    DB.createWorkbookSheet conn wbs
    allWbs <- getAllWorkbookSheets conn
    printTimed $ "set 1: " ++ (show allWbs)
    let sid2 = T.pack "sheetid2"
    let wbs2 = WorkbookSheet "workbookname" [Sheet sid2 "sheetname2" (Blacklist [])]
    DB.createWorkbookSheet conn wbs2
    allWbs' <- getAllWorkbookSheets conn
    printTimed $ "set 2: " ++ (show allWbs')