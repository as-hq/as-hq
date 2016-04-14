{-# LANGUAGE QuasiQuotes, DataKinds, ViewPatterns, TemplateHaskell, OverloadedStrings #-}

module Main where
  
import Prelude()
import AS.Prelude
 
import AS.Config.Settings as S

import AS.Types.Messages
import AS.Types.Network
import AS.Types.Window

import AS.Config.Constants

import AS.Clients()
import AS.Logging
import AS.Util
import AS.DB.API as DB
import AS.DB.Graph as G
import AS.DB.Internal as DI
import AS.DB.Users 
import AS.Users as US
import AS.Handlers.Import (handleImportBinary)
import AS.Handlers.LogAction
import qualified AS.Kernels.Python.Client as KP
import qualified AS.Kernels.R.Client as KR

import AS.Async

import System.FilePath.Posix ((</>))
import System.Directory (createDirectoryIfMissing)
import System.Posix.Signals
import Control.Exception
import Control.Concurrent
import Control.Lens hiding ((.=))

import Data.Aeson hiding (Success)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import Data.String (fromString)

import Data.FileEmbed (embedDir)

-- often want to use these while debugging
-- import Text.Read (readMaybe)
-- import Text.ParserCombinators.Parsec (parse)

import qualified Network.WebSockets as WS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static

import System.Remote.Monitoring as Monitor

import Language.R.Instance as R

-------------------------------------------------------------------------------------------------------------------------
-- Main

main :: IO ()
main = alphaMain $ R.withEmbeddedR R.defaultConfig $ do
  installHandler sigPIPE Ignore Nothing `seq` return ()
  state <- initApp
  -- initializations
  curState <- readMVar state
  when isDebug $ initDebug state

  putsConsole "Recomputing DAG..."
  G.recomputeAllDAGs (curState^.dbConn)
  putsConsole "Done."

  port <- S.getSetting S.serverPort
  putsConsole $ "Server started on port " ++ show port
  runServer state

  putsConsole "Server exited."
  closeLog

initApp :: IO (MVar ServerState)
initApp = do
  -- init logging
  forkIO_ runLogger
  -- init paths
  appDir <- S.getSetting S.appDirectory
  createDirectoryIfMissing True (appDir </> S.images_dir)
  putsConsole "Created image directories if they didn't already exist"
  -- init state
  conn <- DI.connectRedis
  state <- newMVar $ emptyServerState conn
  -- init kernels
  putsConsole "initializing kernels..."
  KP.initialize conn >> putsConsole "Python done."
  KR.initialize conn >> putsConsole "R done."
  -- start diagnostics server
  host <- getSetting serverHost
  port <- getSetting ekgPort
  Monitor.forkServer (BC.pack host) port
  return state

-- |  for debugging. Only called if isDebug is true.
initDebug :: MVar ServerState -> IO ()
initDebug _ = do
  putsConsole "Evaluating debug statements..."
  putsConsole "Done."

runServer :: MVar ServerState -> IO ()
runServer mstate = do
  host <- S.getSetting S.serverHost
  port <- S.getSetting S.serverPort
  Warp.runSettings
    (Warp.setHost (fromString host)
      . Warp.setPort port
      $ Warp.defaultSettings)
    $ application mstate

application :: MVar ServerState -> Wai.Application
application state = WaiWS.websocketsOr WS.defaultConnectionOptions wsApp staticApp
  where
    wsApp :: WS.ServerApp
    wsApp pendingConn = do
      conn <- WS.acceptRequest pendingConn 
      putsConsole "Client connected!"
      msg <- WS.receiveData conn -- waits until it receives data
      when (isDebug && shouldPreprocess) $ preprocess conn state
      handshakeAndStart state conn msg
    staticApp :: Wai.Application
    staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "static")

handshakeAndStart ::  MVar ServerState -> WS.Connection -> B.ByteString -> IO ()
handshakeAndStart state wsConn msg =
  case (decode msg :: Maybe FirstMessage) of
    Just (Login auth) -> do -- first message must be user auth
      putsObj "got auth:" auth
      authResult <- US.authenticateUser auth
      case authResult of 
        Right uid -> do
          dbConn <- view dbConn <$> readMVar state
          userClient <- createUserClient dbConn wsConn uid 
          -- If this is a test session, don't log. Otherwise, set logging = true (another preflight request
          -- will shut this off if we're debugging). Also, don't log testing users in the MySQL db
          case auth of 
            TestAuth  -> do 
              handleStopLoggingActions (State state)
            _         -> do 
              startLoggingActions (State state)
              -- Modify the MySQL DB of users
              --updateUserSession (userId userClient) (userSessionId userClient)
          let defaultSid = windowSheetId . view userWindow $ userClient
          let successMsg = ClientMessage auth_message_id $ AuthSuccess uid defaultSid
          sendMessage successMsg wsConn
          catchAny 
            (engageClient userClient state) 
            (handleRuntimeException userClient state)
        Left authErr -> do
          puts $ "Authentication error: " ++ authErr
          let failMsg = ClientMessage auth_message_id $ AuthFailure authErr
          sendMessage failMsg wsConn
    Just StartHeartbeat -> heartbeat wsConn heartbeat_interval
    _ -> do -- first message is neither
      putsObj "First message not a login message, received: " msg
      sendMessage (failureMessage initialization_failure_message_id "Cannot connect") wsConn -- failure messages are not associated with any send message id.

-- | For debugging purposes. Reads in a list of ServerMessages from a file and processes them, as though
-- sent from a frontend. 
preprocess :: WS.Connection -> MVar ServerState -> IO () 
preprocess = $undefined -- not maintained; hasn't worked for some time. (anand 4/13)
  --do
  ---- prepare the preprocessing
  --fileContents <- AS.Prelude.readFile (S.log_dir ++ "client_messages")
  --let fileLinesWithNumbers = zip (L.lines fileContents) [1..]
  --    nonemptyNumberedFileLines =  filter (\(l, _) -> (l /= "") && $head l /= '#') fileLinesWithNumbers

  --forM_ (chunksOf 3 nonemptyNumberedFileLines) $ 
  --  \[(msg,i), (sid, _), (uid, _)] -> do 
  --    let win = Window (T.pack sid) (-1, -1) (-1, -1)
  --        uid' = T.pack uid
  --        -- userId and sessionId's are synonymous here, because mocked clients don't have a concept of multiple sessions
  --        mockUc = UserClient uid' conn win uid'
  --    curState <- readMVar state
  --    when (isNothing $ US.getUserClientBySessionId uid' curState) $ 
  --      modifyMVar_ state (return . addClient mockUc)
  --    processMessage mockUc state ($read msg)

engageClient :: (Client c) => c -> MVar ServerState -> IO ()
engageClient client state = do
  liftIO $ modifyMVar_ state (return . addClient client) -- add client to state
  printWithTime "Client initialized!"
  finally (talk client state) (onDisconnect client state)

-- | Maintains connection until user disconnects
talk :: (Client c) => c -> MVar ServerState -> IO ()
talk client state = forever $ do
  dmsg <- WS.receiveDataMessage (clientConn client)
  putsConsole "=== RECEIVE MESSAGE ===="
  case dmsg of 
    WS.Binary b -> handleImportBinary client (State state) b
    WS.Text msg -> case (eitherDecode msg :: Either String ServerMessage) of
      Right m -> processAsyncWithTimeout client state m
      Left s -> 
        putsError (clientCommitSource client) ("SERVER ERROR: unable to decode message " 
                                              ++ show msg
                                              ++ "\n\n due to parse error: " 
                                              ++ s)

-- Only show relevant messages (not logAction, which clutters stdout)
putsServerMessage :: ServerMessage -> IO ()
putsServerMessage msg = case msg of 
  ServerMessage _ (LogAction _) -> return ()
  m -> putsObj "Received message" m

-- this functions runs an IO action in a forked thread, adds some thread bookkeeping to the server state, and 
-- removes this bookkeeping upon success. Upon timeout, send an "AskTimeout" message to the client. The 
-- client is expected to reply with a message with the same messageId, in order to reconcile the AskTimeout 
-- with the relevant thread. Upon receiving the reply, kill it, and remove it from state.
processAsyncWithTimeout :: (Client c) => c -> MVar ServerState -> ServerMessage -> IO ()
processAsyncWithTimeout c state msg = case clientType c of 
  UserType -> do
    successLock <- newEmptyMVar 
    tid <- timeoutAsync successLock $ \_ -> do
      putsServerMessage msg
      processMessage c state msg
      putsConsole "=== FINISHED PROCESSING MESSAGE ====" 
    modifyMVar_' state $ \curState -> 
      return $ curState & threads %~ (M.insert mid tid)
    putMVar successLock ()
    where
      mid = serverMessageId msg
      act = getServerActionType (serverAction msg)
      timeoutAsync lock f = forkIO $ 
        timeout S.process_message_timeout onTimeout (onSuccess lock) (f ())
      onTimeout = 
        sendMessage 
          (ClientMessage timeout_message_id $ AskTimeout mid act) 
          (clientConn c)
      onSuccess lock = do
        -- block until the first modifyMVar_ above finishes.
        takeMVar lock
        modifyMVar_' state $ \curState -> 
          return $ curState & threads %~ (M.delete mid)
  DaemonType -> void $ forkIO (processMessage c state msg)

handleRuntimeException :: ASUserClient -> MVar ServerState -> SomeException -> IO ()
handleRuntimeException user state e = do
  let logMsg = displayException e
  putStrLn logMsg
  -- don't log CloseRequest exceptions.
  unless (logMsg `elem` ignoredErrorMessages) $ 
    putsError (userCommitSource user) logMsg 
  purgeZombies state
  runServer state

-- | Sometimes, onDisconnect gets interrupted. (Not sure exactly what.) At any rate, 
-- when this happens, a client that's disconnected is still stored in the state. 
-- This function makes sure they get removed from the state. Unsure if this is actually
-- a robust solution. 
purgeZombies :: MVar ServerState -> IO ()
purgeZombies state = do 
  ucs <- view userClients <$> readMVar state
  mapM_ (\uc -> catch (WS.sendTextData (uc^.userConn) ("ACK" :: T.Text)) 
                      (onDisconnect' uc state)) ucs

-- There's gotta be a cleaner way to do this... but for some reason even typecasting 
-- (\e -> onDisconnect uc state) :: (SomeException -> IO()) didn't work...
onDisconnect' :: (Client c) => c -> MVar ServerState -> SomeException -> IO ()
onDisconnect' c state _ = do 
  onDisconnect c state
  putsError (clientCommitSource c) "ZOMBIE KILLED!!"

processMessage :: (Client c) => c -> MVar ServerState -> ServerMessage -> IO ()
processMessage oldClient mstate message = do
  state <- readMVar mstate 
  let newClient = lookupClient oldClient state -- the client's properties might have been mutated; read it anew
  isPermissible <- DB.isPermissibleMessage (ownerName newClient) (state^.dbConn) message
  if isPermissible || isDebug
    then handleServerMessage newClient (State mstate) message
    else sendMessage (failureMessage (serverMessageId message) "Insufficient permissions") (clientConn newClient)

onDisconnect :: (Client c) => c -> MVar ServerState -> IO ()
onDisconnect c state = do
  putsConsole $ "Client disconnected: " ++ T.unpack (ownerName c)
  liftIO $ modifyMVar_' state (return . removeClient c) -- remove client from server
