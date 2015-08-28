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

import Data.Maybe (fromJust)

import AS.Types
import AS.Config.Settings as S
import AS.Util
import qualified Data.List as L
import AS.Handler as H

-------------------------------------------------------------------------------------------------------------------------
-- | State functions

newServerState :: ServerState
newServerState = State []

numUsers :: ServerState -> Int
numUsers = length . userList 

addToAL :: Eq key => [(key, elt)] -> key -> elt -> [(key, elt)]
addToAL l key value = (key, value) : delFromAL l key

delFromAL :: Eq key => [(key, a)] -> key -> [(key, a)]
delFromAL l key = L.filter (\a -> (fst a) /= key) l

-------------------------------------------------------------------------------------------------------------------------
-- | User Management

getUsers :: ServerState -> [ASUser]
getUsers (State s) = L.map fst s

userIdExists :: ASUserId -> ServerState -> Bool
userIdExists uid state = L.elem uid (L.map userId (getUsers state))

-- Assumes that user exists
getUserById :: ASUserId -> ServerState -> Maybe ASUser
getUserById uid (State allUsers) = case (filter (\c -> (userId c == uid)) (L.map fst allUsers)) of
  [] -> Nothing
  l -> Just $ L.head l

addUser :: ASUser -> ServerState -> ServerState
addUser user state@(State users) = state'
  where
    l = L.lookup user users
    state' = case l of 
      Nothing -> State ((user,[]):users)
      Just _ -> state

close :: WS.Connection -> IO ()
close conn = WS.sendClose conn ("Bye" :: Text)

removeUser :: ASUser -> ServerState -> IO ServerState -- need to deal with daemons 
removeUser user s@(State us) = do 
  let daemons = L.lookup user us
  case daemons of 
    Nothing -> return s
    Just d -> do 
      mapM_ close (L.map daemonConn d)
      let us' = delFromAL us user
      return $ State us'


-------------------------------------------------------------------------------------------------------------------------
-- | Daemon Management

addDaemon :: ASUserId -> ASDaemon -> ServerState -> ServerState
addDaemon uid daemon s@(State state) = state'
  where
    user = getUserById uid s
    state' = case user of 
      Nothing -> s
      Just u -> State $ addToAL state u (daemon:(fromJust $ L.lookup u state))

removeDaemon :: ASDaemon -> ServerState -> ServerState
removeDaemon daemon s@(State state) = state'
  where
    user = getUserOfDaemon s daemon
    state' = case user of 
      Nothing -> s
      Just u -> State $ addToAL state u (L.delete daemon (fromJust $ L.lookup u state))

getUserOfDaemon :: ServerState -> ASDaemon -> (Maybe ASUser)
getUserOfDaemon (State s) daemon = do 
  let users = L.filter (\(a,b) -> L.elem daemon b) s
  case users of 
    [] -> Nothing
    otherwise -> Just $ fst $ L.head users

-------------------------------------------------------------------------------------------------------------------------
-- | Start and end connections

catchDisconnect :: ASUser -> MVar ServerState -> SomeException -> IO ()
catchDisconnect user state e = case (fromException e) of
  Just WS.ConnectionClosed -> do 
    putStrLn $ "in connection closed catch"
    liftIO $ modifyMVar_ state (\s -> removeUser user s)
  otherwise -> (putStrLn (show e)) >> return ()

catchDisconnectDaemon :: ASDaemon -> MVar ServerState -> SomeException -> IO ()
catchDisconnectDaemon daemon state e = case (fromException e) of
  Just WS.ConnectionClosed -> do 
    putStrLn $ "in connection closed catch daemon"
    liftIO $ modifyMVar_ state (\s -> return $ removeDaemon daemon s)
  otherwise -> (putStrLn (show e)) >> return ()

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

isInitConnectionDaemon :: B.ByteString -> IO Bool
isInitConnectionDaemon msg = do
  putStrLn $ "TESTING FOR INIT DAEMON CONNECTION " ++ (show msg)
  b <- case (decode msg :: Maybe ASMessage) of 
        Just (Message _ Acknowledge r (PayloadDaemonInit (ASInitDaemonConnection i l))) -> do
          printTimed "decoded init" 
          return True
        otherwise -> putStrLn (show (decode msg :: Maybe ASMessage)) >> return False
  putStrLn (show msg)
  return b

-------------------------------------------------------------------------------------------------------------------------
-- | Basic send

send :: ASMessage -> WS.Connection -> IO ()
send msg conn = WS.sendTextData conn (encode msg)

-------------------------------------------------------------------------------------------------------------------------
-- | Main 

main :: IO ()
main = do
    state <- newMVar newServerState
    putStrLn $ "server started on port " ++ (show S.wsPort)
    WS.runServer S.wsAddress S.wsPort $ application state
    putStrLn $ "DONE WITH MAIN"

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  -- WS.forkPingThread conn 30 -- Keepalive ping for certain browsers
  msg <- WS.receiveData conn
  -- | After receiving data from a connection, see if it's a valid initial signal (for a daemon or a client)
  isInit <- isInitConnection msg
  isInitDaemon <- isInitConnectionDaemon msg
  if isInit
    then handleInitConnection state conn (decode msg :: Maybe ASMessage)
    else 
      if isInitDaemon
        then handleInitConnectionDaemon state conn (decode msg :: Maybe ASMessage)
        else send (failureMessage "Cannot connect") conn
  putStrLn $ "DONE WITH APPLICATION"

handleInitConnection :: MVar ServerState -> WS.Connection -> Maybe ASMessage -> IO ()
handleInitConnection state conn (Just message) = do
  let user = User (messageUserId message) conn [initialViewingWindow]
  (flip catch) (catchDisconnect user state) $ do
    putStrLn $ "IN HANDLE INIT CONNECTION"
    liftIO $ modifyMVar_ state (\s -> return $ addUser user s)
    talk state user

handleInitConnectionDaemon :: MVar ServerState -> WS.Connection -> Maybe ASMessage -> IO ()
handleInitConnectionDaemon state conn (Just (Message _ _ _ (PayloadDaemonInit (ASInitDaemonConnection pId loc)))) = do
  let daemon = ASDaemon loc conn
  (flip catch) (catchDisconnectDaemon daemon state) $ do
    putStrLn $ "IN HANDLE INIT DAEMON CONNECTION"
    liftIO $ modifyMVar_ state (\s -> return $ addDaemon pId daemon s)
    talkDaemon state daemon

-- Persistent connection until user disconnects
talk :: MVar ServerState -> ASUser -> IO ()
talk state user = forever $ do
  msg <- WS.receiveData (userConn user)
  putStrLn "=========================================================="
  printTimed $ "SERVER message received: " 
  case (decode msg :: Maybe ASMessage) of 
    Nothing -> printTimed ("SERVER ERROR: unable to decode message " ++ (show msg)) >> return ()
    Just m -> do 
      printTimed ("SERVER decoded message: " ++ (show m)) 
      processMessage user state m

talkDaemon :: MVar ServerState -> ASDaemon -> IO ()
talkDaemon state daemon = forever $ do
  msg <- WS.receiveData (daemonConn daemon)
  putStrLn "=========================================================="
  printTimed $ "SERVER message received: " 
  case (decode msg :: Maybe ASMessage) of 
    Nothing -> printTimed ("SERVER ERROR: unable to decode message " ++ (show msg)) >> return ()
    Just m -> do 
      s <- readMVar state
      let user = fromJust $ getUserOfDaemon s daemon
      printTimed ("SERVER decoded message: " ++ (show m)) 
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
  {-UpdateWindow -> do
    liftIO $ modifyMVar_ state $ \s@(State us)-> do
      let c = fromJust $ getUserById (userId user) s
      let (PayloadW vw) = payload message
      let s1 = removeUser c s
      let s2 = State ((User (userId c) (userConn c) (vw:(userWindows c))):us) ls
      return s2 -}
    -- TODO send cells for scroll






