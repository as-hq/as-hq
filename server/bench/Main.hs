{-# LANGUAGE DeriveGeneric, StandaloneDeriving, OverloadedStrings #-}

module Main where

import Prelude

import AS.Types.Network
import AS.Types.DB
import AS.Types.Cell
import AS.Types.CellProps
import AS.Types.Eval
import AS.Types.Messages
import AS.Types.DB
import AS.Types.RowColProps

import AS.Dispatch.Core 
import qualified AS.DB.API as DB
import qualified AS.DB.Graph as G
import qualified AS.DB.Internal as DI
import AS.Window
import AS.Util
import qualified AS.Kernels.Python.Eval as KP

import qualified Database.Redis as R
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import qualified Data.Serialize as S
import qualified Data.Map as M
import qualified Data.HashMap as H
import qualified Data.HashTable.IO as HI
import Data.Hashable
import Control.Monad.Trans.Either
import Control.Applicative
import Control.Concurrent (newMVar, MVar)
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import Debug.Trace
import GHC.Generics

import Criterion.Main

deriving instance Generic ServerState
deriving instance Generic ASUserClient
deriving instance Generic ASDaemonClient

instance NFData ASServerMessage where rnf = genericRnf
instance NFData ASPayload where rnf = genericRnf
instance NFData ASResult where rnf = genericRnf
instance NFData ASCell where rnf = genericRnf
instance NFData ASCellProps where rnf = genericRnf
instance NFData ASExpression where rnf = genericRnf
instance NFData ExpandingType where rnf = genericRnf
instance NFData ASLanguage where rnf = genericRnf
instance NFData CellPropType where rnf = genericRnf
instance NFData ASWindow where rnf = genericRnf
instance NFData WorkbookSheet where rnf = genericRnf
instance NFData ASWorkbook where rnf = genericRnf
instance NFData CellProp where rnf = genericRnf
instance NFData RangeKey where rnf = genericRnf
instance NFData ASSheet where rnf = genericRnf
instance NFData ASPermissions where rnf = genericRnf
instance NFData ASAction where rnf = genericRnf
instance NFData ASCommit where rnf = genericRnf
instance NFData VAlignType where rnf = genericRnf
instance NFData Dimensions where rnf = genericRnf
instance NFData RowColProp where rnf = genericRnf
instance NFData Stream where rnf = genericRnf
instance NFData ASUserEntity where rnf = genericRnf
instance NFData CellDiff where rnf = genericRnf
instance NFData RangeDescriptor where rnf = genericRnf
instance NFData ASUserGroup where rnf = genericRnf
instance NFData StreamSource where rnf = genericRnf
instance NFData ASTime where rnf = genericRnf
instance NFData HAlignType where rnf = genericRnf
instance NFData RowColType where rnf = genericRnf
instance NFData Bloomberg where rnf = genericRnf
instance NFData RowCol where rnf = genericRnf
instance NFData FormatType where rnf = genericRnf
instance NFData JSONField where rnf = genericRnf
instance NFData DescriptorDiff where rnf = genericRnf
instance NFData JSONValue where rnf = genericRnf
instance NFData QueryList where rnf = genericRnf
instance NFData ASRowColProps where rnf = genericRnf
instance NFData MutateType where rnf = genericRnf
instance NFData RowColPropType where rnf = genericRnf
instance NFData Direction where rnf = genericRnf
instance NFData CondFormatRule where rnf = genericRnf
instance NFData ASReplValue where rnf = genericRnf
instance NFData ASInitDaemonConnection where rnf = genericRnf
instance NFData ASInitConnection where rnf = genericRnf
instance NFData ASUserClient where rnf = genericRnf
instance NFData ASDaemonClient where rnf = genericRnf
instance NFData ServerState where rnf = genericRnf
instance NFData ASExecError where rnf = genericRnf

-- don't really give a shit about deep evaluating these types...
instance NFData WS.Connection where rnf conn = seq conn ()
instance NFData R.Connection where rnf conn = seq conn ()
instance NFData (MVar a) where rnf x = seq x ()

instance (NFData k, NFData v) => NFData (HI.IOHashTable HI.HashTable k v) where
  rnf h = seq h ()
instance Hashable ASIndex

data ASEnv = ASEnv { envConn :: R.Connection, envState :: MVar ServerState, envSource :: CommitSource } deriving (Generic)
instance NFData ASEnv where rnf = genericRnf

testCells :: Int -> [ASCell]
testCells n = map (\i -> testCell { cellLocation = Index "BENCH_ID" (1,i) }) [1..n]

emptyEvaluate    = nf id ()
it               = bench
has a b          = env (setupEnvWith a) b 
describe         = bgroup
xdescribe desc _ = trace ("skipped group: " ++ desc) $ bgroup desc []
run :: NFData b => (a -> b) -> a -> Benchmarkable
run = nf
runIO :: NFData a => IO a -> Benchmarkable
runIO = nfIO

setupEnv :: IO ASEnv
setupEnv = do
  conn <- R.connect DI.cInfo
  state <- newMVar $ State [] [] conn (0 :: Port)
  let src = (T.pack "sheetid", T.pack "userid")
  return $ ASEnv conn state src

setupEnvWith :: (NFData a) => a -> IO (ASEnv, a)
setupEnvWith x = (,) <$> setupEnv <*> return x

setCells' :: R.Connection -> [ASCell] -> IO ()
setCells' conn cs = R.runRedis conn $ do
  mapM_ (\c -> R.set (S.encode . cellLocation $ c) (S.encode c)) cs
  return ()

toMap cs = H.fromList $ zip (map cellLocation cs) cs

testMap = toMap . testCells

mergeCells' :: [ASCell] -> [ASCell] -> [ASCell]
mergeCells' c1 c2 = map snd $ H.toList $ H.union (toMap c1) (toMap c2)

main :: IO ()
main = do
  defaultMain [

    xdescribe "dispatch"
      [ has (testCells 500) $ \ ~(myEnv, cells) ->
          it "dispatches 500 cells" $ 
            runIO $ runDispatchCycle (envState myEnv) cells DescendantsWithParent (envSource myEnv)
      ]

    , has (testCells 1000) $ \ ~(_, cells) -> 
        xdescribe "serialization"
          [ it "serializes 10000 cells with cereal" $ 
              run (map S.encode) cells 
          , it "serializes 1000 cells with bytestrings" $ 
              run (BC.pack . show) cells 
          , has (map S.encode cells) $ \ ~(_, scells) -> 
              it "deserializes 1000 cells" $ 
                run (map (\c -> S.decode c :: Either String ASCell)) scells
          ]

    , has ((testMap 10000, testCells 10000)) $ \ ~(_, (m, cells)) -> 
        describe "misc cell datastructures"
          [ it "creates 10000-cell map" $
              run (\cs -> M.fromList $ zip (map cellLocation cs) cs) cells
          , it "creates 10000-cell hashmap" $
              run (\cs -> H.fromList $ zip (map cellLocation cs) cs) cells
          --, it "inserts 10000 cells 10into a hashtable" $
              --runIO $ HI.fromList $ zip (map cellLocation cells) cells
          , it "inserts 10000 cells into a map" $ 
              run (\cs -> insertMultiple (M.empty) (map cellLocation cs) cs) cells
          , it "inserts 10000 cells into a map" $ 
              run (\cs -> insertMultiple (M.empty) (map cellLocation cs) cs) cells
          , has (reverse cells) $ \ ~(_, rcells) -> 
              describe "merging cells" 
              [ it "merges two lists using hashmaps" $ 
                  run (\(c1, c2) -> mergeCells' c1 c2) (cells, rcells)  
              ]
          ]

    , has (testCells 10000) $ \ ~(myEnv, cells) -> 
        xdescribe "DB"
          [ it "inserts 10000 cells with binary serialization" $ 
              runIO $ (setCells' (envConn myEnv) cells) 
          , it "inserts 10000 cells with bytestrings" $ 
              runIO $ (DB.setCells cells)
          , it "sets the ancestors of 10000 cells" $ 
              runIO $ runEitherT $ G.setCellsAncestors cells
          ]

    ]