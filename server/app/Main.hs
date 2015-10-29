{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import System.Environment (getArgs)

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
import qualified Data.ByteString.Char8 as BC 
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as L
import qualified Network.WebSockets as WS

import Data.Maybe (fromJust)
import Text.Read (readMaybe)

import qualified Database.Redis as R

import AS.Clients
import AS.Types.Core
import AS.Config.Settings as S
import AS.Util
import AS.Users
import AS.Config.Paths
import AS.DB.API as DB
import AS.DB.Graph as G
import AS.DB.Util as DBU

import AS.Kernels.Python.Eval as KP
import AS.Kernels.LanguageUtils as KL

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

import qualified Foreign.R as R
import Language.R.Instance as R
import Language.R.QQ

import AS.Parsing.Out

-------------------------------------------------------------------------------------------------------------------------
-- Main

main :: IO ()
main = R.withEmbeddedR R.defaultConfig $ do
    -- initializations
    putStrLn "STARTING APP"
    (conn, ports, states) <- initApp
    if isDebug -- set in Settings.hs
      then initDebug conn
      else return ()
    putStrLn $ "server started on ports " ++ (show ports)
    mapM_ (\(port, state) -> WS.runServer S.wsAddress port $ application state) (zip ports states)
    putStrLn $ "DONE WITH MAIN"

initApp :: IO (R.Connection, [Port], [MVar ServerState])
initApp = do
  -- init eval
  mapM_ KL.clearReplRecord [Python] -- clear/write repl record files
  runEitherT $ KP.evaluate "\'test!\'" -- force load C python sources so that first eval isn't slow
  -- init R
  R.runRegion $ do
    -- the app needs sudo to install packages.
    [r|library("rjson")|]
    [r|library("ggplot2")|]
    return ()
  -- init state
  conn <- R.connect DBU.cInfo
  args <- getArgs
  let intArgs = map (\a -> read a :: Int) args
  let ports = case intArgs of 
        [] -> [S.wsDefaultPort]
        _ -> intArgs
  states <- mapM (\p -> newMVar $ State [] [] conn p) ports
  -- init data
  let sheet = Sheet "INIT_SHEET_ID" "Sheet1" (Blacklist [])
  DB.setSheet conn sheet
  DB.setWorkbook conn $ Workbook "Workbook1" ["INIT_SHEET_ID"]

  return (conn, ports, states)

-- | Initializes database with sheets, etc. for debugging mode. Only called if isDebug is true.
initDebug :: R.Connection -> IO ()
initDebug conn = return ()

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending -- initialize connection
  msg <- WS.receiveData conn -- waits until it receives data
  handleFirstMessage state conn msg

handleFirstMessage ::  MVar ServerState -> WS.Connection -> B.ByteString -> IO ()
handleFirstMessage state conn msg =
  case (decode msg :: Maybe ASClientMessage) of
    Just m@(ClientMessage Acknowledge (PayloadInit (ASInitConnection _))) -> do -- first mesage is user init
      user <- initUserFromMessageAndConn m conn
      if (isDebug && shouldPreprocess) then (preprocess user state) else (return ())
      catch (initClient user state) (handleRuntimeException user state)
    Just m@(ClientMessage Acknowledge (PayloadDaemonInit (ASInitDaemonConnection _ _))) -> do -- first message is daemon init
      initClient (initDaemonFromMessageAndConn m conn) state
    otherwise -> do -- first message is neither
      putStrLn "First message not an initialization message"
      sendMessage (failureMessage "Cannot connect") conn

shouldPreprocess :: Bool
shouldPreprocess = False

-- | For debugging purposes. Reads in a list of ClientMessages from a file and processes them, as though
-- sent from a frontend. 
preprocess :: ASUserClient -> MVar ServerState -> IO () 
preprocess user state = do
  cmp <- getClientMessagesPath
  fileContents <- Prelude.readFile cmp
  let fileLinesWithNumbers = zip (L.lines fileContents) [1..]
  let nonemptyNumberedFileLines =  filter (\(l, i) -> (l /= "") && (head l) /= '#') fileLinesWithNumbers
  mapM_ (\(l,i) -> do 
    putStrLn ("PROCESSING LINE " ++ (show i) ++ ": " ++ l)
    processMessage user state (read l)
    putStrLn "\n\n\n\nFINISHED PREVIOUS MESSAGE\n\n\n\n") nonemptyNumberedFileLines


initClient :: (Client c) => c -> MVar ServerState -> IO ()
initClient client state = do
  liftIO $ modifyMVar_ state (\s -> return $ addClient client s) -- add client to state
  finally (talk client state) (onDisconnect client state)

-- | Maintains connection until user disconnects
talk :: (Client c) => c -> MVar ServerState -> IO ()
talk client state = forever $ do
  msg <- WS.receiveData (conn client)
  putStrLn "=========================================================="
  case (decode msg :: Maybe ASClientMessage) of
    Just m  -> printWithTime ("SERVER message received:  " ++ (show msg)) >> processMessage client state m
    Nothing -> printWithTime ("SERVER ERROR: unable to decode message " ++ (show msg)) >> return ()

handleRuntimeException :: ASUserClient -> MVar ServerState -> SomeException -> IO ()
handleRuntimeException user state e = do
  putStrLn ("Runtime error caught: " ++ (show e))
  port <- appPort <$> readMVar state
  WS.runServer S.wsAddress port $ application state

processMessage :: (Client c) => c -> MVar ServerState -> ASClientMessage -> IO ()
processMessage client state message = do
  dbConnection <- fmap dbConn (readMVar state) -- state stores connection to db; pull it out
  isPermissible <- DB.isPermissibleMessage (ownerName client) dbConnection message
  if (isPermissible || isDebug)
    then handleClientMessage client state message
    else sendMessage (failureMessage "Insufficient permissions") (conn client)

onDisconnect :: (Client c) => c -> MVar ServerState -> IO ()
onDisconnect user state = do
  printWithTime "Client disconnected"
  liftIO $ modifyMVar_ state (\s -> return $ removeClient user s) -- remove client from server
