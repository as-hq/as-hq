{-# LANGUAGE OverloadedStrings #-}

module Main where

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
import AS.Config.Settings as S
import AS.Util
import qualified Data.List as L
import AS.Handler as H

-------------------------------------------------------------------------------------------------------------------------
-- | State 

newServerState :: ServerState
newServerState = State [] []

numUsers :: ServerState -> Int
numUsers = length . userList 

-------------------------------------------------------------------------------------------------------------------------
-- | User management

userIdExists :: ASUserId -> ServerState -> Bool
userIdExists uid state = L.elem uid (L.map userId (userList state))

-- Assumes that user exists
getUserById :: ASUserId -> ServerState -> ASUser
getUserById uid (State allUsers _) = L.head $ filter (\c -> (userId c == uid)) allUsers
    
addUser :: ASUser -> ServerState -> ServerState
addUser user state@(State users locs) = if (userIdExists (userId user) state)
  then state
  else (State (user : users) locs)

removeUser :: ASUser -> ServerState -> ServerState
removeUser user (State us ls) = State us' ls
  where
    us' = filter ((/= userId user) . userId) us

-------------------------------------------------------------------------------------------------------------------------
-- | Start and end connections

disconnect :: ASUser -> MVar ServerState -> IO ()
disconnect user state = do 
  ret <- liftIO $ modifyMVar_ state (\s -> return $ removeUser user s)
  return ret

isInitConnection :: B.ByteString -> IO Bool
isInitConnection msg = do
  putStrLn $ "TESTING FOR INIT CONNECTION " ++ (show msg)
  b <- case (decode msg :: Maybe ASMessage) of 
        Just (Message _ Acknowledge r (PayloadInit (ASInitConnection n))) -> do
          printTimed "decoded init" 
          return True
        otherwise -> putStrLn (show (decode msg :: Maybe ASMessage)) >> return False
  putStrLn (show msg)
  return b

send :: ASMessage -> WS.Connection -> IO ()
send msg conn = WS.sendTextData conn (encode msg)

-------------------------------------------------------------------------------------------------------------------------
-- | Main 

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
    else send (failureMessage "Cannot connect") conn

handleInitConnection :: MVar ServerState -> WS.Connection -> Maybe ASMessage -> IO ()
handleInitConnection state conn (Just message) = do
  let user = User (messageUserId message) conn [initialViewingWindow]
  (flip catch) (catchDisconnect user state) $ do
    putStrLn $ "IN HANDLE INIT CONNECTION"
    liftIO $ modifyMVar_ state (\s -> return $ addUser user s)
    talk state user

catchDisconnect :: ASUser -> MVar ServerState -> SomeException -> IO ()
catchDisconnect user state e = case (fromException e) of
  Just WS.ConnectionClosed -> do 
    putStrLn $ "in connection closed catch"
    liftIO $ modifyMVar_ state (\s -> return $ removeUser user s)
  otherwise -> (putStrLn (show e)) >> return ()

-- persistent connection until user disconnects
talk :: MVar ServerState -> ASUser -> IO ()
talk state user' = forever $ do
    msgOrError <- try (WS.receiveData (userConn user')) :: IO (Either SomeException B.ByteString)
    case msgOrError of 
      Left e -> send (failureMessage "Could not receive data") (userConn user') 
      Right msg -> do
        putStrLn "=========================================================="
        printTimed $ "SERVER message received: " -- ++ (B.unpack msg)
        case (decode msg :: Maybe ASMessage) of 
          Nothing -> printTimed "SERVER ERROR: unable to decode message" >> return ()
          Just m -> do 
            printTimed ("SERVER decoded message: " ++ (show m)) 
            -- | Let the user be the one with user id dictacted by the message (useful for streaming daemons)
            s <- readMVar state
            let user =  getUserById (messageUserId m) s 
            processMessage user state m

-------------------------------------------------------------------------------------------------------------------------
-- | Message handling

processMessage :: ASUser -> MVar ServerState -> ASMessage -> IO ()
processMessage user state message = case (action message) of 
  Acknowledge -> WS.sendTextData (userConn user) ("ACK" :: Text)
  Evaluate    -> (H.handleEval user state message) 
  Get         -> H.handleGet user state (payload message)
  Delete      -> H.handleDelete user state (payload message)
  Undo        -> (H.handleUndo user state) >> (printTimed "Server processed undo")
  Redo        -> (H.handleRedo user state) >> (printTimed "Server processed redo")
  Clear       -> H.handleClear user state
  AddTags     -> H.handleAddTags user state message
  RemoveTags  -> H.handleRemoveTags user state message
  UpdateWindow -> do
    liftIO $ modifyMVar_ state $ \s@(State us ls)-> do
      let c = getUserById (userId user) s
      let (PayloadW vw) = payload message
      let s1 = removeUser c s
      let s2 = State ((User (userId c) (userConn c) (vw:(userWindows c))):us) ls
      return s2
    -- TODO send cells for scroll






