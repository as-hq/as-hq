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

numUsers :: ServerState -> Int
numUsers = length

-------- user management ------------------
userExists :: ASUser -> ServerState -> Bool
userExists user = any ((== userId user) . userId)

-- assumes that user exists
getUserById :: MVar ServerState -> ASUserId -> IO ASUser
getUserById state uid = liftIO $ do
    allUsers <- readMVar state
    let f = filter (\c -> (userId c == uid)) allUsers
    return $ L.head f
    
addUser :: ASUser -> ServerState -> ServerState
addUser user users = user : users

removeUser :: ASUser -> ServerState -> ServerState
removeUser user = filter ((/= userId user) . userId)

broadcast :: Text -> ServerState -> IO ()
broadcast message users = do
    putStrLn $ "Broadcast msg " ++ (T.unpack message)
    forM_ users $ \(User _ conn _) -> WS.sendTextData conn message

-- only send a cell to a user if its in their viewing window
broadcastFiltered :: ASMessage -> [ASUser] -> IO ()
broadcastFiltered (Message uid _ _ (PayloadCL cells)) users = mapM_ (sendCells cells) users 
  where
    sendCells :: [ASCell] -> ASUser -> IO ()
    sendCells cells user = do 
      let cells' = intersectViewingWindows cells (userWindows user)
      let msg = Message uid Update Success (PayloadCL cells')
      WS.sendTextData (userConn user) (encode msg)

send :: ASMessage -> WS.Connection -> IO ()
send msg conn = WS.sendTextData conn (encode msg)

disconnect :: ASUser -> MVar ServerState -> IO ()
disconnect user state = do 
  ret <- liftIO $ modifyMVar_ state (\s -> return $ removeUser user s)
  return ret

isInitConnection :: B.ByteString -> IO Bool
isInitConnection msg = do
  b <- case (decode msg :: Maybe ASMessage) of 
        Just (Message _ Acknowledge r (PayloadInit (ASInitConnection n))) -> do
          printTimed "decoded init" 
          return True
        otherwise -> putStrLn (show (decode msg :: Maybe ASMessage)) >> return False
  putStrLn (show msg)
  return b

----------------------------------------------- main ----------------------------------

main :: IO ()
main = do
    state <- newMVar newServerState
    putStrLn $ "server started on port " ++ (show S.wsPort)
    WS.runServer S.wsAddress S.wsPort $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30 -- keepalive ping for certain browsers
  msg <- WS.receiveData conn
  isInit <- isInitConnection msg
  if isInit
    then handleInitConnection state conn (decode msg :: Maybe ASMessage)
    else send failureMessage conn

handleInitConnection :: MVar ServerState -> WS.Connection -> Maybe ASMessage -> IO ()
handleInitConnection state conn (Just message) = do
  let user = User (messageUserId message) conn [initialViewingWindow]
  (flip catch) (catchDisconnect user state) $ do
    liftIO $ modifyMVar_ state (\s -> return $ addUser user s)
    talk state user

catchDisconnect :: ASUser -> MVar ServerState -> SomeException -> IO ()
catchDisconnect user state e = case (fromException e) of
  Just WS.ConnectionClosed -> do 
    putStrLn $ "in connection closed catch"
    liftIO $ modifyMVar_ state (\s -> return $ removeUser user s)

-- persistent connection until user disconnects
talk :: MVar ServerState -> ASUser -> IO ()
talk state user = forever $ do
    msgOrError <- try (WS.receiveData (userConn user)) :: IO (Either SomeException B.ByteString)
    case msgOrError of 
      Left e -> send failureMessage (userConn user)
      Right msg -> do
        putStrLn "=========================================================="
        printTimed $ "SERVER message received: " -- ++ (B.unpack msg)
        case (decode msg :: Maybe ASMessage) of 
          Nothing -> printTimed "SERVER ERROR: unable to decode message" >> return ()
          Just m -> printTimed ("SERVER decoded message: " ++ (show m)) >> processMessage state user m

--------------------------------------- message handling -----------------------------------

processMessage :: MVar ServerState -> ASUser -> ASMessage -> IO ()
processMessage state user message = case (action message) of 
  Acknowledge -> WS.sendTextData (userConn user) ("ACK" :: Text)
  Evaluate  -> do
    result <- DP.handleEval (payload message) user
    liftIO $ do 
      allUsers <- readMVar state
      broadcastFiltered (updateMessageUser (userId user) result) allUsers
  Get       -> do
    result <- DB.handleGet (payload message) 
    printTimed "Handling get"
    send (updateMessageUser (userId user) result) (userConn user)
  Delete    -> do
    result <- DB.handleDelete (payload message) 
    liftIO $ do 
      allUsers <- readMVar state
      broadcastFiltered (updateMessageUser (userId user) result) allUsers
  Undo -> do 
    result <- DB.handleUndo 
    send (updateMessageUser (userId user) result) (userConn user)
    printTimed "Server processed undo"
  Redo -> do 
    result <- DB.handleRedo 
    send (updateMessageUser (userId user) result) (userConn user)
    printTimed "Server processed redo"
  Clear -> do 
    result <- DB.handleClear
    send (updateMessageUser (userId user) result) (userConn user)
    printTimed "Server processed clear"
  UpdateWindow -> do
    liftIO $ modifyMVar_ state $ \s -> do
          c <- getUserById state (userId user)
          let (PayloadW vw) = payload message
          let s1 = removeUser c s
          let s2 = (User (userId c) (userConn c) (vw:(userWindows c))):s1
          return s2
    -- TODO send cells for scroll


