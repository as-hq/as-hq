{-# LANGUAGE OverloadedStrings, QuasiQuotes, DataKinds, TemplateHaskell, ViewPatterns #-}

module Main where
  
import Prelude()
import AS.Prelude
 
import AS.Config.Settings as S

import AS.Types.Messages
import AS.Types.Network

import AS.Clients()
import AS.Logging
import AS.Window
import AS.Util
import AS.Config.Paths
import AS.DB.API as DB
import AS.DB.Graph as G
import AS.DB.Internal as DI
import AS.Users as US
import AS.Handlers.Misc (handleImportBinary)
import AS.Types.Locations
import qualified AS.Kernels.Python.Eval as KP

import System.Posix.Signals
import Control.Exception
import Control.Monad (forever, when)
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Control.Lens hiding ((.=))

import Data.Aeson hiding (Success)
import Data.Maybe hiding (fromJust)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as L
import Data.List.Split (chunksOf)

-- often want to use these while debugging
-- import Text.Read (readMaybe)
-- import Text.ParserCombinators.Parsec (parse)

import qualified Network.WebSockets as WS

import Language.R.Instance as R
import Language.R.QQ


-------------------------------------------------------------------------------------------------------------------------
-- Main

main :: IO ()
main = R.withEmbeddedR R.defaultConfig $ do
  blockSignals $ addSignal sigPIPE emptySignalSet
  -- initializations
  putStrLn "STARTING APP"
  state <- initApp
  curState <- readMVar state
  let settings = curState^.appSettings

  if isDebug -- set in Settings.hs
    then initDebug state
    else return ()

  G.recompute (settings^.graphDbAddress) (curState^.dbConn)
  putStrLn "RECOMPUTED DAG"

  putStrLn $ "server started on port " ++ (show $ settings^.backendWsPort)
  WS.runServer (settings^.backendWsAddress) (settings^.backendWsPort) $ application state
  putStrLn $ "DONE WITH MAIN"

initApp :: IO (MVar ServerState)
initApp = do
  -- init state
  settings <- S.getSettings 
  conn <- DI.connectRedis settings
  state <- newMVar $ State [] [] conn settings 
  -- init python kernel
  KP.initialize (settings^.pyKernelAddress) conn
  -- init R
  R.runRegion $ do
    -- the app needs sudo to install packages.
    [r|library("rjson")|]
    [r|library("ggplot2")|]
    return ()
  -- init data
  let sheet = Sheet "INIT_SHEET_ID" "Sheet1" (Blacklist [])
  DB.setSheet conn sheet
  DB.setWorkbook conn $ Workbook "Workbook1" ["INIT_SHEET_ID"]

  return state

-- |  for debugging. Only called if isDebug is true.
initDebug :: MVar ServerState -> IO ()
initDebug mstate = do
  putStrLn "\n\nEvaluating debug statements..."
  putStrLn "\nDone."
  return ()

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending -- initialize connection
  -- here, we fork a heartbeat thread, to signal to the client that the connection is alive
  forkHeartbeat conn heartbeat_interval
  msg <- WS.receiveData conn -- waits until it receives data
  when (isDebug && shouldPreprocess) $ preprocess conn state
  handleFirstMessage state conn msg

handleFirstMessage ::  MVar ServerState -> WS.Connection -> B.ByteString -> IO ()
handleFirstMessage state conn msg =
  case (decode msg :: Maybe ServerMessage) of
    Just (ServerMessage (Initialize cuid csid)) -> do -- first mesage is user init
      user <- initUser conn cuid csid
      catch (initClient user state) (handleRuntimeException user state)
    Just (ServerMessage (InitializeDaemon parentId parentLoc)) -> do -- first message is daemon init
      let daemon = initDaemonFromMessageAndConn conn parentId parentLoc
      initClient daemon state
    _ -> do -- first message is neither
      putStrLn "First message not an initialization message"
      sendMessage (failureMessage "Cannot connect") conn

-- #needsrefactor should move to Environment.hs, or something
shouldPreprocess :: Bool
shouldPreprocess = False

-- | For debugging purposes. Reads in a list of ServerMessages from a file and processes them, as though
-- sent from a frontend. 
preprocess :: WS.Connection -> MVar ServerState -> IO () 
preprocess conn state = do
  -- prepare the preprocessing
  logDir <- getServerLogDir
  fileContents <- AS.Prelude.readFile (logDir ++ "client_messages")
  let fileLinesWithNumbers = zip (L.lines fileContents) [1..]
      nonemptyNumberedFileLines =  filter (\(l, _) -> (l /= "") && ($head l) /= '#') fileLinesWithNumbers

  mapM_ (\[(msg,i), (sid, _), (uid, _)] -> do 
    putStrLn ("PROCESSING LINE " ++ (show i) ++ ": " ++ msg ++ "\n" ++ sid ++ "\n" ++ uid)
    let win = Window (T.pack sid) (Coord (-1) (-1)) (Coord (-1) (-1))
        cid = T.pack uid
        mockUc = UserClient cid conn win (T.pack "")
    curState <- readMVar state
    when (isNothing $ US.getUserByClientId cid curState) $ liftIO $ modifyMVar_ state (\s -> return $ addClient mockUc s)
    processMessage mockUc state ($read msg)
    putStrLn "\n\n\n\nFINISHED PROCESSING MESSAGE\n\n\n\n") (chunksOf 3 nonemptyNumberedFileLines)
  putStrLn "\n\nFinished preprocessing."

initClient :: (Client c) => c -> MVar ServerState -> IO ()
initClient client state = do
  liftIO $ modifyMVar_ state (\s -> return $ addClient client s) -- add client to state
  putStrLn "Client connected!"
  finally (talk client state) (onDisconnect client state)

-- | Maintains connection until user disconnects
talk :: (Client c) => c -> MVar ServerState -> IO ()
talk client state = forever $ do
  dmsg <- WS.receiveDataMessage (conn client)
  case dmsg of 
    WS.Binary b -> handleImportBinary client state b
    WS.Text msg -> case (eitherDecode msg :: Either String ServerMessage) of
      Right m -> processMessage client state m
      Left s -> printWithTime ("SERVER ERROR: unable to decode message " 
                               ++ (show msg) 
                               ++ "\n\n due to parse error: " 
                               ++ s)

forkHeartbeat :: WS.Connection -> Milliseconds -> IO ()
forkHeartbeat conn interval = forkIO (dieSilently `handle` go 1) >> return ()
  where
    go i = do
      threadDelay (interval * 1000)
      WS.sendTextData conn ("PING" :: T.Text)
      go (i+1)
    dieSilently e = case fromException e of 
      Just asyncErr -> throwIO (asyncErr :: AsyncException) >> return ()
      Nothing       -> return ()

handleRuntimeException :: ASUserClient -> MVar ServerState -> SomeException -> IO ()
handleRuntimeException user state e = do
  let logMsg = "Runtime error caught: " ++ (show e)
  putStrLn logMsg
  logError logMsg (userCommitSource user)
  settings <- view appSettings <$> readMVar state
  purgeZombies state
  WS.runServer (settings^.backendWsAddress) (settings^.backendWsPort) $ application state

-- | Sometimes, onDisconnect gets interrupted. (Not sure exactly what.) At any rate, 
-- when this happens, a client that's disconnected is still stored in the state. 
-- This function makes sure they get removed from the state. Unsure if this is actually
-- a robust solution. 
purgeZombies :: MVar ServerState -> IO ()
purgeZombies state = do 
  ucs <- view userClients <$> readMVar state
  (flip mapM_) ucs (\uc -> catch (WS.sendTextData (userConn uc) ("ACK" :: T.Text)) 
                                 (onDisconnect' uc state))

-- There's gotta be a cleaner way to do this... but for some reason even typecasting 
-- (\e -> onDisconnect uc state) :: (SomeException -> IO()) didn't work...
onDisconnect' :: (Client c) => c -> MVar ServerState -> SomeException -> IO ()
onDisconnect' user state _ = do 
  onDisconnect user state
  printWithTime "ZOMBIE KILLED!! [mgao machine gun sounds]\n"

processMessage :: (Client c) => c -> MVar ServerState -> ServerMessage -> IO ()
processMessage client state message = do
  dbConnection <- view dbConn <$> readMVar state -- state stores connection to db; pull it out
  isPermissible <- DB.isPermissibleMessage (ownerName client) dbConnection message
  if (isPermissible || isDebug)
    then handleServerMessage client state message
    else sendMessage (failureMessage "Insufficient permissions") (conn client)

onDisconnect :: (Client c) => c -> MVar ServerState -> IO ()
onDisconnect user state = do
  printWithTime "Client disconnected"
  liftIO $ modifyMVar_ state (\s -> return $ removeClient user s) -- remove client from server
