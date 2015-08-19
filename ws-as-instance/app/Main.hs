{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib

import Prelude
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception 
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson hiding (Success)
import Data.ByteString.Char8 hiding (putStrLn,filter,any,length)
import Data.ByteString.Lazy.Char8 as B hiding (putStrLn,filter,any,length)

import qualified Network.WebSockets as WS

import AS.Types
import AS.Dispatch as DP
import AS.DB as DB
import AS.Config.Settings as S
import AS.Util
import qualified Data.List as L



------- state ------------------------------
newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

-------- client management ------------------
clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== clientName client) . clientName)

-- assumes that client exists
getClientByName :: MVar ServerState -> Text -> IO Client
getClientByName state name = liftIO $ do
    allClients <- readMVar state
    let f = filter (\c -> (clientName c == name)) allClients
    return $ L.head f
    
addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= clientName client) . clientName)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    putStrLn $ "Broadcast msg " ++ (T.unpack message)
    forM_ clients $ \(Client _ conn _) -> WS.sendTextData conn message

-- only send a cell to a client if its in their viewing window
broadcastFiltered :: ASMessage -> Client -> [Client] -> IO ()
broadcastFiltered (Message _ _ (PayloadCL cells)) user clients = mapM_ (send cells) clients 
  where
    send :: [ASCell] -> Client -> IO ()
    send cells client = do 
      let cells' = intersectViewingWindow cells (clientVW client)
      let msg = Message Evaluate Success (PayloadCL cells')
      WS.sendTextData (clientConn client) (T.pack (B.unpack (encode msg)))
broadcastFiltered msg user clients = do 
  WS.sendTextData (clientConn user) (T.pack (B.unpack (encode msg)))
  

disconnect :: Client -> MVar ServerState -> IO ()
disconnect client state = do 
  ret <- liftIO $ modifyMVar_ state $ \s -> do
      let s' = removeClient client s
      return s'
  return ret

isInitConnection :: B.ByteString -> IO Bool
isInitConnection msg = do
  b <- case (decode msg :: Maybe ASMessage) of 
        Just (Message Acknowledge r (PayloadInit (ASInitConnection n))) -> (putStrLn "decoded init") >> return True
        otherwise -> putStrLn (show (decode msg :: Maybe ASMessage)) >> return False
  putStrLn (show msg)
  return b

getUsername :: B.ByteString -> Text
getUsername msg = n
  where
    Just (Message Acknowledge r (PayloadInit (ASInitConnection n))) = (decode msg :: Maybe ASMessage)


-------- main ----------------------------------
main :: IO ()
main = do
    state <- newMVar newServerState
    putStrLn $ "server started on port " ++ (show S.wsPort)
    WS.runServer S.wsAddress S.wsPort $ application state

catchDisconnect :: Client -> MVar ServerState -> SomeException -> IO ()
catchDisconnect client state e = case (fromException e) of
  Just WS.ConnectionClosed -> do 
    putStrLn $ "in connection closed catch"
    ret <- liftIO $ modifyMVar_ state $ \s -> do
      let s' = removeClient client s
      return s'
    return ret
  otherwise -> return ()

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  msg <- WS.receiveData conn
  init <- isInitConnection (B.pack (T.unpack msg))
  if (init)
    then do 
      let client = Client (getUsername (B.pack (T.unpack msg))) conn initialViewingWindow
      (flip catch) (catchDisconnect client state) $ do
        liftIO $ modifyMVar_ state $ \s -> do
          let s' = addClient client s
          broadcast (clientName client) s'
          return s'
        talk state client
    else 
      return ()

-- persistent connection until client disconnects
talk :: MVar ServerState -> Client -> IO ()
talk state client = forever $ do
    msgOrError <- try (WS.receiveData (clientConn client)) :: IO (Either SomeException B.ByteString)
    case msgOrError of 
      Left e -> return ()
      Right msg -> do
        putStrLn "=========================================================="
        printTime $ "SERVER message received: " -- ++ (B.unpack msg)
        case (decode msg :: Maybe ASMessage) of 
          Nothing -> printTime "SERVER ERROR: unable to decode message" >> return ()
          Just m -> printTime ("SERVER decoded message: " ++ (show m)) >> processMessage state client m

------------------- message handling ---------------------------
processMessage :: MVar ServerState -> Client -> ASMessage -> IO ()
processMessage state client message = case (action message) of 
  Acknowledge -> WS.sendTextData (clientConn client) ("ACK" :: Text)
  Evaluate  -> do
    result <- DP.handleEval (payload message) client
    liftIO $ do 
      allClients <- readMVar state
      broadcastFiltered result client allClients
  Get       -> do
    -- | update state with viewing window
    _ <- liftIO $ modifyMVar_ state $ \s -> do
          c <- getClientByName state (clientName client)
          let vw = vWindow (payload message)
          let s1 = removeClient c s
          let s2 = (Client (clientName c) (clientConn c) vw):s1
          return s2
    result <- DB.handleGet (payload message) 
    liftIO $ do 
      allClients <- readMVar state
      broadcast (T.pack (B.unpack (encode result))) allClients
  Delete    -> do
    result <- DB.handleDelete (payload message) 
    liftIO $ do 
      allClients <- readMVar state
      broadcast (T.pack (B.unpack (encode result))) allClients
  Undo -> do 
    result <- DB.handleUndo 
    _ <- WS.sendTextData (clientConn client) (T.pack (B.unpack (encode result)))
    putStrLn $ "Server processed undo"
  Redo -> do 
    result <- DB.handleRedo 
    _ <- WS.sendTextData (clientConn client) (T.pack (B.unpack (encode result)))
    putStrLn $ "Server processed redo"
  Clear -> do 
    result <- DB.handleClear
    _ <- WS.sendTextData (clientConn client) (T.pack (B.unpack (encode result)))
    putStrLn $ "Server processed clear"
