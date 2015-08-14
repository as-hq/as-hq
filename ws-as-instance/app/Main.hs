{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib

import Prelude
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson
import Data.ByteString.Lazy.Char8 as B (pack, unpack)

import qualified Network.WebSockets as WS

import AS.Types
import AS.Dispatch as DP
import AS.DB as DB
import AS.Config.Settings as S
import AS.Util

------- types ------------------------------
type Client = (Text, WS.Connection)

type ServerState = [Client]

------- state ------------------------------
newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

-------- client management ------------------
clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn message
    forM_ clients $ \(_, conn) -> WS.sendTextData conn message

-- TODO handle client disconnecting
onDisconnect :: IO ()
onDisconnect = return ()

-------- main ----------------------------------
main :: IO ()
main = do
    state <- newMVar newServerState
    putStrLn $ "server started on port " ++ (show S.wsPort)
    WS.runServer S.wsAddress S.wsPort $ application state

-- We also fork a pinging thread in the background. This will ensure the connection
-- stays alive on some browsers.
application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  flip finally onDisconnect $ talk conn

-- persistent connection until client disconnects
talk :: WS.Connection -> IO ()
talk conn = forever $ do
    msg <- WS.receiveData conn
    putStrLn "=========================================================="
    printTimed $ "SERVER message received: " -- ++ (B.unpack msg)
    case (decode msg :: Maybe ASMessage) of 
      Nothing -> printTimed "SERVER ERROR: unable to decode message" >> return ()
      Just m -> printTimed ("SERVER decoded message: " ++ (show m)) >> processMessage conn m

------------------- message handling ---------------------------
processMessage :: WS.Connection -> ASMessage -> IO ()
processMessage conn message = case (action message) of 
  Acknowledge -> WS.sendTextData conn ("ACK" :: Text)
  Evaluate  -> do
    result <- DP.handleEval (payload message)
    printTimed $ "SERVER result: " ++ (show result)
    WS.sendTextData conn (encode result)
  Get       -> do
    result <- DB.handleGet (payload message)
    printTimed $ "SERVER result: " ++ (show result)
    WS.sendTextData conn (encode result)
  Delete    -> do
    result <- DB.handleDelete (payload message)
    WS.sendTextData conn (encode result)


