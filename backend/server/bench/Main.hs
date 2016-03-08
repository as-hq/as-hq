{-# LANGUAGE DeriveGeneric, StandaloneDeriving, OverloadedStrings, BangPatterns, TemplateHaskell #-}

module Main where

import Lib

import Prelude()
import AS.Prelude


import AS.Types.Network
import AS.Types.DB
import AS.Types.Cell
import AS.Types.CellProps
import AS.Types.Eval
import AS.Types.Messages
import AS.Types.DB
import AS.Handlers.Paste (performCopy)
import AS.Types.Commits

import AS.Dispatch.Core 
import qualified AS.DB.API as DB
import qualified AS.DB.Graph as G
import qualified AS.DB.Internal as DI
import AS.Util
import qualified AS.Kernels.Python as KP
import qualified AS.Serialize as S

import qualified Database.Redis as R
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.HashMap as H
import qualified Data.HashTable.IO as HI

import Control.Monad.Trans.Either
import Control.Lens hiding (has)
import Criterion.Main (defaultMain)
import Control.Concurrent

import AS.Config.Settings

setTestCellsInDB :: [Int] -> IO ()
setTestCellsInDB = (DI.connectRedis >>=) . flip DB.setCells . testCells

main :: IO ()
main = alphaMain $ do
  setTestCellsInDB [1]

  defaultMain [
    describe "dispatch"
      [ has (testCells [1..1000]) $ \ ~(myEnv, cells) ->
          it "dispatches 1000 cells" $ 
            runIO $ do 
              curState <- readMVar $ envState myEnv 
              runDispatchCycle curState cells DescendantsWithParent (envSource myEnv) id
      ]

    , has (testCells [1..1000]) $ \ ~(_, cells) -> 
        xdescribe "serialization"
          [ it "serializes 1000 cells with cereal" $ 
              run (map S.encode) cells 

          , it "serializes 1000 cells with bytestrings" $ 
              run (BC.pack . show) cells 

          , has (map S.encode cells) $ \ ~(_, scells) -> 
              it "deserializes 1000 cells" $ 
                run (map ($fromRight . S.decode) :: [B.ByteString] -> [ASCell]) (scells :: [B.ByteString])
          ]

    , has ((testMap [1..10000], testCells [1..10000])) $ \ ~(_, (m, cells)) -> 
        xdescribe "misc cell datastructures"
          [ it "creates 10000-cell map" $
              run (\cs -> M.fromList $ zip (mapCellLocation cs) cs) cells

          , it "creates 10000-cell hashmap" $
              run (\cs -> H.fromList $ zip (mapCellLocation cs) cs) cells

          , it "inserts 10000 cells into a map" $ 
              run (\cs -> insertMultiple (M.empty) (mapCellLocation cs) cs) cells

          , has (reverse cells) $ \ ~(_, rcells) -> 
              describe "merging cells" 
              [ it "merges two lists using maps" $ 
                  run (\(c1, c2) -> mergeCells c1 c2) (cells, rcells)  
              ]
          ]

    , has (testCells [1..10000], testCells [10001..20000]) $ \ ~(myEnv, (cells1, cells2)) -> 
        describe "DB"
          [ it "inserts 10000 cells with binary serialization" $ 
              runIO $ DB.setCells (envConn myEnv) cells1 

          , it "gets all cells after having inserted 10000" $ 
              runIO $ DB.getAllCells (envConn myEnv)

          , it "sets the ancestors of 10000 cells" $ 
              runIO $ runEitherT $ G.setCellsAncestors cells1

          , it "deletes all cells in sheet BENCH_ID" $ 
              runIO $ DB.deleteLocsInSheet (envConn myEnv) "BENCH_ID"
          ]

    , describe "python kernel"
      [ it "evaluates a simple expression using the new kernel" $ 
          runIO $ KP.testCell "INIT_SHEET_ID" "1+1"

      , it "evaluates range(10000)" $ 
          runIO $ KP.testCell "INIT_SHEET_ID" "range(10000)"
      ]

    , has () $ \ ~(myEnv, _) -> 
        xdescribe "copy/paste" 
          [ it "copies 20x20 grid" $
              runIO $ do
                curState <- readMVar $ envState myEnv 
                performCopy curState (Range "BENCH_ID" ((Coord 1 1),(Coord 1 1))) (Range "BENCH_ID" ((Coord 1 1), (Coord 20 20))) (CommitSource "BENCH_ID" "")
          ]
    ]