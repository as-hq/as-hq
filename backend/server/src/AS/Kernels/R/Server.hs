{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AS.Kernels.R.Server where

import AS.Prelude 
import AS.Config.Settings

import AS.Serialize as Serial
import AS.Kernels.R.Types
import AS.Kernels.R.Shell
import AS.Logging (getTime)
import AS.Util
import AS.DB.API (getAllHeaders)
import AS.DB.Internal (connectRedis)
import AS.Types.EvalHeader
import AS.Types.Messages
import AS.Types.Sheets
import AS.Types.Cell (ASLanguage(..))

import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe (isNothing)
import Data.List.NonEmpty (fromList)
import Safe (headMay)

import Control.Concurrent
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.Catch as MC
import Control.Exception

import qualified System.ZMQ4 as Z
import System.ZMQ4.Monadic

import qualified Language.R.Instance as R
import qualified Foreign.R as R
import Foreign.R.Type as R
import Language.R.QQ

import System.Posix.Types
import System.Posix.Signals
import System.Directory (createDirectoryIfMissing)
import System.Random (randomRIO)

--------------------------------------------------------------------------------
-- Constants 

empty_frame         = ""
ready               = "READY"
url_workers         = "ipc:///tmp/rkernel_workers"

--------------------------------------------------------------------------------
-- Helpers

idGen :: MonadIO m => m B.ByteString
idGen = pack <$> liftIO getUUID

--------------------------------------------------------------------------------
-- Server initialization

-- | Initialize namespaces with header values.
initialize :: IO ()
initialize = do
  dbConn <- connectRedis
  headers <- getAllHeaders dbConn R
  url <- getSetting rkernelAddress_client
  -- forkProcess because we don't want to share ZMQ context with parent process
  forkProcess' $ do
    Z.withContext $ \ctx -> 
      Z.withSocket ctx Z.Dealer $ \client -> do
        Z.connect client url
        forM_ headers $ \header -> do
          let code = header^.evalHeaderExpr
              wid = header^.evalHeaderWorkbookId
              msg = EvaluateRequest Header "initialize_message_id" wid code
          Z.send' client [] $ Serial.encodeLazy msg
  putStrLn "\n\nFinished initialization.\n\n"

--------------------------------------------------------------------------------
-- Running the server

serverLoop :: MVar KernelState -> Socket z Router -> Socket z Router -> ZMQ z ()
serverLoop state frontend backend = 
  handleAny onException $ 
    forever $ do 
      [evtsB, evtsF] <- poll (-1) [Sock backend [In] Nothing, 
                                   Sock frontend [In] Nothing]
      processBackend  state (backend, frontend) evtsB 
      processFrontend state (frontend, backend) evtsF 
  where
    onException e = do
      puts state $ "ERROR: " ++ (show (e :: SomeException))
      serverLoop state frontend backend 

-- | Main server loop. Caveat: requires that R has already been initialized.
runServer :: Addr -> Int -> IO ()
runServer url_clients numInitialWorkers = do
  -- Initialize logging
  logHandle <- getLogHandle "rkernel"

  -- Initialize state
  state <- newMVar $ newKernelState logHandle
  initialize

  -- initialize R libraries
  -- the app needs to be run as root to install packages.
  R.runRegion $ void $
    [r|
      library("rjson")
      library("ggplot2")
    |] 

   -- Close resources cleanly upon Ctrl+C
  let sigintHandler = do
          puts state "Kernel exited normally."
          flushLog state
          closeLog state
          killWorkers state
          raiseSignal sigTERM
  installHandler sigINT (Catch sigintHandler) Nothing

  runZMQ $ do
    puts state $ "\n\nServer listening on " ++ url_clients
    flushLog state

    frontend <- socket Router
    bind frontend url_clients
    backend <- socket Router
    bind backend url_workers

    serverLoop state frontend backend 

--------------------------------------------------------------------------------
-- Process backend router events

-- | Handle worker activity on backend. There are two cases:
-- 1) The backend router received a registration message from a worker. In this
-- case, we just register the worker in state and do nothing else.
-- 2) The backend router received an evaluation reply from a worker, which
-- it will forward to the frontend router. From runWorkbookWorker, we received three
-- frames. The req socket padded an empty_frame frame and the backend router has 
-- padded the worker's network, so we really have five frames upon reception.
-- We send back [clientAddr, reply] to the frontend router, which will
-- convey the result to the client by stripping away the clientAddr and sending
-- [reply] to the client at clientAddr.
processBackend  :: (Receiver r, Sender r, Sender s) 
                => MVar KernelState           -- shared kernel state
                -> (Socket z r, Socket z s)   -- backend, frontend sockets
                -> [Event]                    -- polled events
                -> ZMQ z ()
processBackend state (backend, frontend) evts = 
  when (In `elem` evts) $ do
    msg <- receiveMulti backend
    if (msg !! 2 == ready) 
      then do
        let netid = msg !! 0
        let wid = T.pack . unpack $ msg !! 4
        liftIO $ registerWorkbookWorker state wid netid
        puts state $ "WORKER REGISTERED: " ++ show wid
        dequeueMsgs state backend wid 
      else do
        let clientAddr = msg !! 2
        let reply = msg !! 4
        putsTimed state "==== SENT BACK REPLY ==== "
        sendMulti frontend $ fromList [clientAddr, reply]
        flushLog state

--------------------------------------------------------------------------------
-- Process frontend router events

-- | The frontend router received a message from the client. At this point, 
-- the frontend router has automatically padded the request with the clientAddr.
-- 1) The request is a poke, in which case we handle it
-- 2) The request is an eval request from the client, but we don't have an
-- available worker. Make some more workers and recurse with the same events.
-- 3) The request is an eval request from the client and we have an available
-- worker. Send a five-frame message to backend consisting of the worker's 
-- network id, the client address, and the decoded request. 
processFrontend :: forall z. forall r. forall s. 
                   (Receiver r, Sender r, Sender s) 
                => MVar KernelState         -- shared state
                -> (Socket z r, Socket z s) -- frontend, backend sockets
                -> [Event]                  -- polled events
                -> ZMQ z ()
processFrontend  state (frontend, backend) evts = 
  when (In `elem` evts) $ do
    msg <- receiveMulti frontend
    let [clientAddr, req] = msg

    let forwardRequest :: WorkbookWorker -> ZMQ z ()
        forwardRequest w = 
          sendMulti backend $ fromList [ fromJust (w^.networkId)
                                       , empty_frame, clientAddr
                                       , empty_frame, req
                                       ]

    let reply :: KernelReply -> ZMQ z ()
        reply r = sendMulti frontend $ 
          fromList [clientAddr, empty_frame, Serial.encode r]

    case (Serial.decode req) of 
      Right er@(EvaluateRequest _ _ wid _) -> do
        worker <- liftIO $ getWorkbookWorker state wid
        case worker of  
          Nothing -> liftIO $ do
            puts state "Creating workbook worker."
            -- queue message for nonexistent worker
            queueMsg state wid [clientAddr, req]
            installWorkbookWorker state wid 
          Just w -> 
            if (w^.isRegistered)
              then do
                -- Invariant: if the worker is registered, it has a valid Network ID
                forwardRequest w
              else do
                -- queue message for unregistered worker
                liftIO $ queueMsg state wid [clientAddr, req]

      -- broadcast halt request to all workbook workers
      Right (HaltMessageRequest {}) -> do
        broadcastToWorkers state backend [clientAddr, req]

      Right (GetStatusRequest {}) -> 
        broadcastToWorkers state backend [clientAddr, req]

      Right (ClearRequest wid) -> do
        worker <- liftIO $ getWorkbookWorker state wid
        whenJust worker forwardRequest

      Left e -> do
        putsTimed state $ "Could not decode request: " ++ show e
        reply $ GenericErrorReply e

--------------------------------------------------------------------------------
-- Worker functions

-- | Create and install a worker. Its status is 'Unregistered' until it 
-- sends a registration message.
installWorkbookWorker :: MVar KernelState -> WorkbookID -> IO ()
installWorkbookWorker state wid = do
  pid <- forkProcess' $ runWorkbookWorker wid 
  let worker = WorkbookWorker 
                { _workerWorkbookId = wid
                , _networkId = Nothing
                , _process = pid
                , _isRegistered = False
                }
  modifyMVar_' state $ 
    return . (& workbookWorkers %~ M.insert wid worker)

-- | Upon reception of a worker registration message, update its status and 
-- network id in state.
registerWorkbookWorker :: MVar KernelState -> WorkbookID -> NetworkId -> IO ()
registerWorkbookWorker state wid netid = 
  modifyMVar_' state $ 
    return . (& workbookWorkers %~ M.adjust 
      ( (& networkId .~ Just netid)
      . (& isRegistered .~ True))
      wid)

-- | Get a worker that is not engaged.
getWorkbookWorker :: MVar KernelState -> WorkbookID -> IO (Maybe WorkbookWorker)
getWorkbookWorker state wid = readMVar state >>= \s -> 
  return (M.lookup wid $ s^.workbookWorkers)  

-- | Queue an evalution request for an unregistered worker.
queueMsg :: MVar KernelState -> WorkbookID -> Message -> IO ()
queueMsg state wid msg = 
  modifyMVar_' state $ 
    return . (& messageQueue %~ M.alter queueIt wid)
  where 
    queueIt q = case q of 
      Nothing -> Just [msg]
      Just q  -> Just $ msg:q

-- | Dump the message queue when the workbook worker has registered.
dequeueMsgs :: (Sender s) => MVar KernelState -> Socket z s -> WorkbookID -> ZMQ z ()
dequeueMsgs state backend wid = do
  st <- liftIO $ readMVar state
  whenJust (M.lookup wid $ st^.messageQueue) $ \q -> 
    let netid = view networkId $ (st^.workbookWorkers) M.! wid 
    in do 
      liftIO $ modifyMVar_' state $ 
        return . (& messageQueue %~ M.delete wid)
      forM_ q $ \[clientAddr, msg] -> 
        sendMulti backend $ fromList  [ fromJust netid
                                      , empty_frame, clientAddr
                                      , empty_frame, msg
                                      ]

broadcastToWorkers :: (Sender s) => MVar KernelState -> Socket z s -> Message -> ZMQ z ()
broadcastToWorkers state backend [clientAddr, msg] = do
  st <- liftIO $ readMVar state
  forM_ (M.elems $ st^.workbookWorkers) $ \worker -> 
    when (worker^.isRegistered) $ 
      sendMulti backend $ fromList  [ fromJust (worker^.networkId)
                                    , empty_frame, clientAddr
                                    , empty_frame, msg
                                    ]

killWorkers :: MVar KernelState -> IO ()
killWorkers state = readMVar state >>= \s -> 
  forM_ (M.elems $ s^.workbookWorkers) $ \worker -> 
    signalProcess sigKILL $ worker^.process

--------------------------------------------------------------------------------
-- Main worker routine

-- | Run worker which pocesses all evaluations on a single workbook.
runWorkbookWorker :: WorkbookID -> IO ()
runWorkbookWorker wid = bracket Z.context Z.term $ \c -> do
  -- Dealer is used because the worker doesn't reply to every message it receives
  -- (e.g. when halting a previous message, only the workbook worker with the message
  -- being worked on will return a reply)
  Z.withSocket c Z.Dealer $ \backend -> do
    let workerId = pack . T.unpack $ wid
    Z.setIdentity (restrict workerId) backend
    Z.connect backend url_workers

    -- send registration message
    Z.sendMulti backend $ fromList [empty_frame, ready, empty_frame, workerId]

    handle <- getLogHandle $ "rkernel-workbook-" ++ T.unpack wid
    let env = R.unsexp . R.cast R.SEnv $ [rsafe| new.env() |]
    state <- newMVar $ newWorkbookState env handle

    forever $ do
      -- blocks here until the worker is registered 
      -- (due to invariant: unregistered worker never assigned work)
      ["", clientAddr, "", msg] <- Z.receiveMulti backend
      putsTimed state $ "==== WORKER " ++ T.unpack wid ++ " RECEIVED MESSAGE ==== "

      let req = Serial.decode msg
      let mid = case req of 
                Right (EvaluateRequest _ m _ _) -> Just m
                _ -> Nothing

      let reply :: KernelReply -> IO ()
          reply r = Z.sendMulti backend $ 
            fromList [empty_frame, clientAddr, empty_frame, Serial.encode r]

      case req of 
        -- serialize all header evaluations
        -- blocks workbook worker thread
        Right (EvaluateRequest Header _ _ code) -> do
          env <- getWorkbookEnv state
          rep <- EvaluateReply <$> runBlock Header env code
          reply rep
          
        -- fork cell evaluations
        Right (EvaluateRequest Cell mid _ code) -> do
          env <- getWorkbookEnv state
          workerPid <- forkProcess' $ do
            rep <- EvaluateReply <$> runBlock Cell env code
            bracket Z.context Z.term $ \c -> 
              Z.withSocket c Z.Req $ \backend -> do
                Z.connect backend url_workers
                -- will be delivered to backend (and not the workbook workers), because
                -- the backend socket is the only one calling "bind"
                Z.sendMulti backend $ fromList [clientAddr, empty_frame, Serial.encode rep]
          recordWorkerPid state mid workerPid

        Right (HaltMessageRequest mid) -> do
          worker <- lookupWorker state mid
          whenJust worker $ \w -> do
            signalProcess sigKILL $ w^.cellProcess
            clearWorkerPid state $ w^.cellProcess
            reply GenericSuccessReply

        Right (GetStatusRequest mid) -> do
          worker <- lookupWorker state mid
          whenJust worker $ \_ -> 
            reply StillProcessingReply

        Right (ClearRequest {}) -> do
          let env = R.unsexp . R.cast R.SEnv $ [rsafe| new.env() |]
          modifyMVar_' state $ 
            return . (& workbookEnvironment .~ env)
          reply GenericSuccessReply

        Right r -> error $ "Sheet worker received non-evaluation request: " ++ show r

        Left err -> do
          putsTimed state $ "Could not decode request: " ++ show err 
          reply $ GenericErrorReply err

    flushLog state

lookupWorker :: MVar WorkbookState -> MessageId -> IO (Maybe CellWorker)
lookupWorker state mid = do
  workers <- M.elems . view cellWorkers <$> readMVar state
  return $ find ((== mid) . view cellMessage) workers

recordWorkerPid :: MVar WorkbookState -> MessageId -> ProcessID -> IO ()
recordWorkerPid state mid pid = 
  let worker = CellWorker { _cellProcess = pid, _cellMessage = mid }
  in modifyMVar_' state $ 
      return . (& cellWorkers %~ M.insert pid worker)

clearWorkerPid :: MVar WorkbookState -> ProcessID -> IO ()
clearWorkerPid state pid = 
  modifyMVar_' state $ 
    return . (& cellWorkers %~ M.delete pid)

getWorkbookEnv :: MVar WorkbookState -> IO R.SEXP0
getWorkbookEnv state = view workbookEnvironment <$> readMVar state 

--------------------------------------------------------------------------------
-- Process messages

getLogHandle :: String -> IO (Maybe Handle)
getLogHandle logName = do
  logsOn <- getSetting rkernelLogsOn 
  if logsOn 
    then do
      createDirectoryIfMissing True rkernel_logs_dir
      logFile <- ((rkernel_logs_dir ++ "[" ++ logName ++ "]") ++) <$> getTime
      Just <$> openFile logFile AppendMode 
    else 
      return Nothing