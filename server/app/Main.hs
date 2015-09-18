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
import qualified Data.List as L
import qualified Network.WebSockets as WS

import Data.Maybe (fromJust)

import qualified Database.Redis as R

import AS.Types
import AS.Config.Settings as S
import AS.Util
import AS.Clients
import AS.DB.API as DB
import AS.DB.Util as DBU
import AS.Handler as H

-------------------------------------------------------------------------------------------------------------------------
-- | State functions

numUsers :: ServerState -> Int
numUsers = length . userList 

-------------------------------------------------------------------------------------------------------------------------
-- | Daemon Management

addDaemon :: ASUserId -> ASDaemon -> ServerState -> ServerState
addDaemon uid daemon s@(State users conn) = state'
  where
    user = getUserById uid s
    state' = case user of 
      Nothing -> s
      Just u -> flip State conn $ addToAL users u (daemon:(fromJust $ L.lookup u users))

removeDaemon :: ASDaemon -> ServerState -> ServerState
removeDaemon daemon s@(State state conn) = state'
  where
    user = getUserOfDaemon s daemon
    state' = case user of 
      Nothing -> s
      Just u -> flip State conn $ addToAL state u (L.delete daemon (fromJust $ L.lookup u state))

getUserOfDaemon :: ServerState -> ASDaemon -> (Maybe ASUser)
getUserOfDaemon (State s _) daemon = do 
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
        otherwise -> do 
          putStrLn (show (decode msg :: Maybe ASMessage))
          return False
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
-- | Main

main :: IO ()
main = do
    -- initializations
    conn <- R.connect DBU.cInfo
    state <- newMVar $ State [] conn
    if isDebug -- set in Settings.hs 
      then initDebug conn >> return ()
      else return ()
    -- where the heavy lifting is done
    WS.runServer S.wsAddress S.wsPort $ application state
    putStrLn $ "server started on port " ++ (show S.wsPort)
    putStrLn $ "DONE WITH MAIN"

-- | Initializes database with sheets, etc. for debugging mode. Only called if isDebug is true. 
initDebug :: R.Connection -> IO ()
initDebug conn = do
  let sheetid = T.pack "SHEET_ID"
      sheetid2 = T.pack "SHEET_ID2"
  DB.setSheet conn $ Sheet sheetid "SHEET_NAME" (Blacklist [])
  DB.setWorkbook conn $ Workbook "WORKBOOK_NAME" [sheetid]
  DB.setSheet conn $ Sheet sheetid2 "SHEET_NAME" (Blacklist [])
  DB.setWorkbook conn $ Workbook "WORKBOOK_NAME2" [sheetid2]
  return  ()

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending -- initialize connection
  msg <- WS.receiveData conn -- waits until it receives data
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
    -- adds client to state
    liftIO $ modifyMVar_ state (\s -> return $ addUser user s)
    -- handles all future exchanges with this client
    talk state user

handleInitConnectionDaemon :: MVar ServerState -> WS.Connection -> Maybe ASMessage -> IO ()
handleInitConnectionDaemon state conn (Just (Message _ _ _ (PayloadDaemonInit (ASInitDaemonConnection pId loc)))) = do
  let daemon = ASDaemon loc conn
  (flip catch) (catchDisconnectDaemon daemon state) $ do
    liftIO $ modifyMVar_ state (\s -> return $ addDaemon pId daemon s)
    talkDaemon state daemon

-- | Persistent connection until user disconnects
talk :: MVar ServerState -> ASUser -> IO ()
talk state user = forever $ do
  msg <- WS.receiveData (userConn user)
  putStrLn "=========================================================="
  printTimed "SERVER message received: " 
  case (decode msg :: Maybe ASMessage) of 
    Just m  -> processMessage user state m
    Nothing -> printTimed ("SERVER ERROR: unable to decode message " ++ (show msg)) >> return ()

talkDaemon :: MVar ServerState -> ASDaemon -> IO ()
talkDaemon state daemon = forever $ do
  msg <- WS.receiveData (daemonConn daemon)
  putStrLn "=========================================================="
  printTimed "SERVER message received: " 
  case (decode msg :: Maybe ASMessage) of 
    Just m -> do 
      s <- readMVar state
      let user = fromJust $ getUserOfDaemon s daemon
      processMessage user state m

-------------------------------------------------------------------------------------------------------------------------
-- | Message handling

processMessage :: ASUser -> MVar ServerState -> ASMessage -> IO ()
processMessage user state message = do
  conn <- fmap dbConn (readMVar state) -- state stores connection to db; pull it out
  isPermissible <- DB.isPermissibleMessage conn (userId user) message
  if (isPermissible || isDebug)
    then handleMessage user state message
    else send (failureMessage "Insufficient permissions") (userConn user)

handleMessage :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleMessage user state message = case (action message) of 
  Acknowledge -> WS.sendTextData (userConn user) ("ACK" :: Text)
  New         -> H.handleNew user state message
  Import      -> H.handleImport user state message
  Open        -> H.handleOpen user state message
  Close       -> H.handleClose user state message
  Evaluate    -> H.handleEval user state message 
  EvaluateRepl-> H.handleEvalRepl user state message
  Get         -> H.handleGet user state (payload message)
  Delete      -> H.handleDelete user state (payload message)
  Copy        -> H.handleCopy user state (payload message)
  CopyForced  -> H.handleCopyForced user state (payload message)
  Undo        -> (H.handleUndo user state) >> (printTimed "Server processed undo")
  Redo        -> (H.handleRedo user state) >> (printTimed "Server processed redo")
  Clear       -> H.handleClear user state
  AddTags     -> H.handleAddTags user state message
  RemoveTags  -> H.handleRemoveTags user state message
  UpdateWindow-> H.handleUpdateWindow user state message
  {-UpdateWindow -> do
    liftIO $ modifyMVar_ state $ \s@(State us)-> do
      let c = fromJust $ getUserById (userId user) s
      let (PayloadW vw) = payload message
      let s1 = removeUser c s
      let s2 = State ((User (userId c) (userConn c) (vw:(userWindows c))):us) ls
      return s2 -}
    -- TODO send cells for scroll






