{-# LANGUAGE OverloadedStrings #-}

import AS.Types.Core
import AS.Types.DB
import AS.DB.API as DB
import AS.DB.Util as DU
import AS.Util

import Data.Text as T
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
    testSetCells
    printTimed "cells set"
    cells <- testGetCells
    printTimed $ "got cells: " ++ (show cells)

testSetCells :: IO ()
testSetCells = do
    let cells = testCells 10
    --printTimed $ L.concat $ L.map show2 cells
    DB.setCells cells

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