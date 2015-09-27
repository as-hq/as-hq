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

import AS.Clients
import AS.Types.Core
import AS.Config.Settings as S
import AS.Util
import AS.Users
import AS.DB.API as DB
import AS.DB.Util as DBU

-------------------------------------------------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
    -- initializations
    conn <- R.connect DBU.cInfo
    state <- newMVar $ State [] [] conn -- server state
    if isDebug -- set in Settings.hs 
      then initDebug conn >> return ()
      else return ()
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
  handleFirstMessage state conn msg 

handleFirstMessage ::  MVar ServerState -> WS.Connection -> B.ByteString -> IO ()
handleFirstMessage state conn msg = do
  case (decode msg :: Maybe ASMessage) of 
    Just m@(Message _ Acknowledge _ (PayloadInit (ASInitConnection _))) -> do -- first mesage is user init
      user <- initUserFromMessageAndConn m conn
      initClient user state 
    Just m@(Message _ Acknowledge _ (PayloadDaemonInit (ASInitDaemonConnection _ _))) -> do -- first message is daemon init
      initClient (fromJust $ initDaemonFromMessageAndConn m conn) state
    otherwise -> do -- first message is neither
      putStrLn "First message not an initialization message"
      sendMessage (failureMessage "Cannot connect") conn

initClient :: (Client c) => c -> MVar ServerState -> IO ()
initClient client state = do 
  liftIO $ modifyMVar_ state (\s -> return $ addClient client s) -- add client to state
  catch (talk state client) (catchDisconnect client state) -- ::ALEX:: should the liftIO be inside the catch ? 

-- | Maintains connection until user disconnects
talk :: (Client c) => MVar ServerState -> c -> IO ()
talk state client = forever $ do
  msg <- WS.receiveData (conn client)
  putStrLn "=========================================================="
  case (decode msg :: Maybe ASMessage) of 
    Just m  -> printTimed ("SERVER message received:  " ++ (show msg)) >> processMessage client state m
    Nothing -> printTimed ("SERVER ERROR: unable to decode message " ++ (show msg)) >> return ()

-------------------------------------------------------------------------------------------------------------------------
-- Message handling

processMessage :: (Client c) => c -> MVar ServerState -> ASMessage -> IO ()
processMessage client state message = do
  dbConnection <- fmap dbConn (readMVar state) -- state stores connection to db; pull it out
  isPermissible <- DB.isPermissibleMessage dbConnection message
  if (isPermissible || isDebug)
    then handleClientMessage client state message
    else sendMessage (failureMessage "Insufficient permissions") (conn client)

catchDisconnect :: (Client c) => c -> MVar ServerState -> SomeException -> IO ()
catchDisconnect user state e = case (fromException e) of
  Just WS.ConnectionClosed -> do 
    putStrLn $ "\n\n\nin connection closed catch\n\n\n"
    liftIO $ modifyMVar_ state (\s -> return $ removeClient user s) -- remove client from server state
  otherwise -> (putStrLn (show e)) >> return ()