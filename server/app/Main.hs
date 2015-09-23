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
import AS.ClientsImplementations
import AS.Config.Settings as S
import AS.Util
import AS.Clients
import AS.DB.API as DB
import AS.DB.Util as DBU
import AS.Handler as H

-------------------------------------------------------------------------------------------------------------------------
-- | State functions

numUsers :: ServerState -> Int
numUsers = length . userClients

-------------------------------------------------------------------------------------------------------------------------
-- | Daemon Management

-- ::ALEX:: add attribute to Daemon later?? 
getUserOfDaemon :: ServerState -> ASDaemon -> (Maybe ASUser)
getUserOfDaemon (State s _ _) daemon = Nothing 

-------------------------------------------------------------------------------------------------------------------------
-- | Start and end connections

catchDisconnect :: (Client c) => c -> MVar ServerState -> SomeException -> IO ()
catchDisconnect user state e = case (fromException e) of
  Just WS.ConnectionClosed -> do 
    putStrLn $ "in connection closed catch"
    liftIO $ modifyMVar_ state (\s -> return $ removeClient user s)
  otherwise -> (putStrLn (show e)) >> return ()

isInitConnection :: B.ByteString -> IO Bool
isInitConnection msg = do
  putStrLn $ "TESTING FOR INIT CONNECTION " ++ (show msg)
  b <- case (decode msg :: Maybe ASMessage) of 
        Just (Message _ Acknowledge r (PayloadInit (ASInitConnection))) -> do
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
    state <- newMVar $ State [] [] conn
    if isDebug -- set in Settings.hs 
      then initDebug conn >> return ()
      else return ()
    -- where the heavy lifting is done
    putStrLn $ "server started on port " ++ (show S.wsPort)
    WS.runServer S.wsAddress S.wsPort $ application state
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
  if isInit
    then handleInitConnection state conn (decode msg :: Maybe ASMessage)
    else send (failureMessage "Cannot connect") conn
  putStrLn $ "DONE WITH APPLICATION" -- ::ALEX:: what??

handleInitConnection :: MVar ServerState -> WS.Connection -> Maybe ASMessage -> IO ()
handleInitConnection state conn (Just message) = do
  case (initDaemonFromMessageAndConn message conn) of 
    Just daemon -> something daemon state
    Nothing -> something (initUserFromMessageAndConn message conn) state

something :: (Client c) => c -> MVar ServerState -> IO ()
something client state = (flip catch) (catchDisconnect client state) $ do
    -- adds client to state
    liftIO $ modifyMVar_ state (\s -> return $ addClient client s)
    -- handles all future exchanges with this client
    talk state client


-- | Persistent connection until user disconnects
talk :: (Client c) => MVar ServerState -> c -> IO ()
talk state client = forever $ do
  msg <- WS.receiveData (conn client)
  putStrLn "=========================================================="
  printTimed "SERVER message received: " 
  case (decode msg :: Maybe ASMessage) of 
    Just m  -> processMessage client state m
    Nothing -> printTimed ("SERVER ERROR: unable to decode message " ++ (show msg)) >> return ()

-------------------------------------------------------------------------------------------------------------------------
-- | Message handling

processMessage :: (Client c) => c -> MVar ServerState -> ASMessage -> IO ()
processMessage client state message = do
  conn <- fmap dbConn (readMVar state) -- state stores connection to db; pull it out
  let isPermissible = True -- <- DB.isPermissibleMessage conn (userId user) message -- ::ALEX::
  if (isPermissible || isDebug)
    then handleMessage client state message
    else (return ()) -- send (failureMessage "Insufficient permissions") (userConn client)

  {-UpdateWindow -> do
    liftIO $ modifyMVar_ state $ \s@(State us)-> do
      let c = fromJust $ getUserById (userId user) s
      let (PayloadW vw) = payload message
      let s1 = removeUser c s
      let s2 = State ((User (userId c) (userConn c) (vw:(userWindows c))):us) ls
      return s2 -}
    -- TODO send cells for scroll