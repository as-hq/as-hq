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
import AS.Types.Cell

import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe (isNothing)
import Data.List.NonEmpty (fromList)
import System.ZMQ4.Monadic
import Safe (headMay)
import Control.Lens
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.Catch as MC
import Control.Exception

import qualified Language.R.Instance as R
import Language.R.QQ

import System.Posix.Signals
import System.Directory (createDirectoryIfMissing)
import System.Random (randomRIO)

--------------------------------------------------------------------------------
-- Constants 

-- | The number of additional workers created when high load is detected
load_response_delta = 5

empty               = ""
ready               = "READY"
url_workers         = "inproc://myworkers"

--------------------------------------------------------------------------------
-- Helpers

idGen :: MonadIO m => m B.ByteString
idGen = pack <$> liftIO getUniqueId

--------------------------------------------------------------------------------
-- Server initialization

-- | Initialize namespaces with header values.
initialize :: MVar State -> IO ()
initialize state = do
  dbConn <- connectRedis
  headers <- getAllHeaders dbConn R
  forM_ headers $ \header -> 
    let code = header^.evalHeaderExpr
        sid = header^.evalHeaderSheetId
    in runBlock state code Header sid

--------------------------------------------------------------------------------
-- Running the server

serverLoop :: Socket z Router -> Socket z Router -> MVar State -> Bool -> ZMQ z ()
serverLoop frontend backend state logsOn = catchAny
  (forever $ do 
    [evtsB, evtsF] <- poll (-1) [Sock backend [In] Nothing, 
                                 Sock frontend [In] Nothing]
    processBackend  state (backend, frontend) evtsB logsOn
    processFrontend state (frontend, backend) evtsF logsOn
  ) 
  (\e -> do 
    puts state $ "ERROR: " ++ (show (e :: SomeException))
    serverLoop frontend backend state logsOn
  )

-- | Main server loop. Caveat: requires that R has already been initialized.
runServer :: Addr -> Int -> IO ()
runServer url_clients numInitialWorkers = do
   -- Initialize logging
  logsOn <- getSetting rkernelLogsOn 
  logHandle <- if logsOn
    then do 
      createDirectoryIfMissing True rkernel_logs_dir
      logFile <- ((rkernel_logs_dir ++ "[rkernel]") ++) <$> getTime
      Just <$> openFile logFile AppendMode 
    else return Nothing

  -- Initialize state
  shell <- newShell
  state <- newMVar $ emptyState shell logHandle
  
  -- r kernel will NOT initialize itself, because the main 
  -- backend server is responsible for this and we shouldn't double-initialize.
  -- (for now). anand 4/18
  -- initialize state

  -- Close log cleanly upon Ctrl+C
  let sigintHandler = do
          putStrLn "Exited normally."
          when logsOn $ do
            flushLog state
            closeLog state
          raiseSignal sigTERM
  seq (installHandler sigINT (Catch sigintHandler) Nothing) (return ())

  runZMQ $ do
    puts state "Starting workers..."
   
    replicateM_ numInitialWorkers $ 
      installWorker state =<< idGen

    puts state $ "Server listening on " ++ url_clients
    when logsOn $ flushLog state

    frontend <- socket Router
    bind frontend url_clients
    backend <- socket Router
    bind backend url_workers

    serverLoop frontend backend state logsOn 

    puts state "Kernel exited."


--------------------------------------------------------------------------------
-- Worker initialization

-- | initialize namespaces with header values
initialize :: MVar State -> IO ()
initialize state = do
  dbConn <- connectRedis
  headers <- getAllHeaders dbConn R
  forM_ headers $ \header -> 
    let code = header^.evalHeaderExpr
        sid = header^.evalHeaderSheetId
    in runBlock state code Header sid

-- | Create and install a worker. Its status is 'Unregistered' until it 
-- sends a registration message.
installWorker :: MVar State -> WorkerId -> ZMQ z ()
installWorker state wid = do
  lock <- liftIO newEmptyMVar
  thread <- async $ liftIO (takeMVar lock) >>= \_ -> runWorker state wid
  let worker = Worker wid Nothing thread Unregistered
  liftIO $ do
    modifyMVar_' state $ 
      return . (& workers %~ M.insert wid worker) . (& numWorkers %~ (1+))
    putMVar lock ()

-- | Upon reception of a worker registration message, update its status and 
-- network id in state.
registerWorker :: MVar State -> WorkerId -> NetworkId -> IO ()
registerWorker state wid netid = 
  modifyMVar_' state $ 
    return . (& workers %~ M.update 
      (Just . (& networkId .~ Just netid)
            . (& status .~ Idle))
      wid)

--------------------------------------------------------------------------------
-- Worker util methods

-- | Get a worker that is not engaged.
getAvailableWorker :: MVar State -> IO (Maybe Worker)
getAvailableWorker state = readMVar state >>= (\s ->
  let isIdle w = (w^.status) `elem` [Idle, Unregistered]
  in return $ headMay . M.elems $ M.filter isIdle (s^.workers))

setWorkerStatus :: MVar State -> WorkerId -> WorkerStatus -> IO ()
setWorkerStatus state wid s = modifyMVar_' state $ 
  return . (& workers %~ M.update 
    (Just . (& status .~ s))
    wid)

--------------------------------------------------------------------------------
-- Main worker routine

-- | Workers are just Req sockets. First, we send a ready message to the 
-- backend router, which updates state. In the below forever loop,
-- the backend router has received an eval request from the frontend router. It 
-- had five frames (case 3 of processFrontend), and the backend router 
-- automatically saved and stripped the first one (worker network id). 
-- The worker Req socket strips the second empty frame. Thus, when we 
-- receiveMulti, we get the clientAddr, empty, and decoded message frames. 
-- We manage worker business while we're evaluating.
--
-- When we're done processing, we send a three-frame response
-- to the backend router consisting of clientAddr, empty, encoded response.
runWorker :: MVar State -> WorkerId -> ZMQ z ()
runWorker state wid = do
  sock <- socket Req
  setIdentity (restrict wid) sock
  connect sock url_workers
  sendMulti sock $ fromList [ready, empty, wid]

  logsOn <- liftIO $ getSetting rkernelLogsOn

  forever $ do
    -- blocks here until the worker is registered 
    -- (due to invariant: unregistered worker never assigned work)
    [clientAddr, "", msg] <- receiveMulti sock
    putsTimed state $ "==== WORKER " ++ unpack wid ++ " RECEIVED MESSAGE ==== "

    let req = Serial.decode msg
    let mid = case req of 
              Right (EvaluateRequest _ m _ _) -> Just m
              _ -> Nothing

    -- Set status to busy while doing work
    liftIO $ setWorkerStatus state wid (Busy mid)

    rep <- case req of 
      Right myReq -> do
        putsTimed state $ "Processing request: " ++ show myReq
        processMessage state myReq
      Left err -> do
        putsTimed state "Could not decode request"
        return $ GenericErrorReply err

    sendMulti sock $ fromList [clientAddr, empty, Serial.encode rep]

    -- Build up logs in buffer during work, then flush to file during each 
    -- worker's dead zone (the period between having finished work and becoming 
    -- non-idle). This is a tradeoff between increasing evaluation time and 
    -- increasing the number of workers, favoring the latter.
    when logsOn $ flushLog state

    -- Reset idle status
    liftIO $ setWorkerStatus state wid Idle

--------------------------------------------------------------------------------
-- Process backend router events

-- | Handle worker activity on backend. There are two cases:
-- 1) The backend router received a registration message from a worker. In this
-- case, we just register the worker in state and do nothing else.
-- 2) The backend router received an evaluation reply from a worker, which
-- it will forward to the frontend router. From runWorker, we received three
-- frames. The req socket padded an empty frame and the backend router has 
-- padded the worker's network, so we really have five frames upon reception.
-- We send back [clientAddr, reply] to the frontend router, which will
-- convey the result to the client by stripping away the clientAddr and sending
-- [reply] to the client at clientAddr.
processBackend :: (Receiver r, Sender s) => 
                  MVar State -> -- shared worker state
                  (Socket z r, Socket z s) -> -- backend, frontend sockets
                  [Event] -> -- polled events
                  Bool -> -- whether logs are enabled
                  ZMQ z ()
processBackend state (backend, frontend) evts logsOn = when (In `elem` evts) $ do
  msg <- receiveMulti backend
  if (msg !! 2 == ready) 
    then liftIO $ do
      let netid = msg !! 0
      let wid = msg !! 4
      registerWorker state wid netid
      puts state $ "WORKER REGISTERED: " ++ show wid
    else do
      let clientAddr = msg !! 2
      let reply = msg !! 4
      putsTimed state "==== SENT BACK REPLY ==== "
      sendMulti frontend $ fromList [clientAddr, reply]
      when logsOn $ flushLog state

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
--
-- This does decoding twice currently; once here and again in runWorker. If this
-- becomes a speed problem, all we really need is isGetStatusRequest.
processFrontend :: (Receiver r, Sender r, Sender s) => 
                   MVar State -> -- shared state
                   (Socket z r, Socket z s) -> -- frontend, backend sockets
                   [Event] -> -- polled events
                   Bool -> -- whether logs are enabled
                   ZMQ z ()
processFrontend  state (frontend, backend) evts logsOn = 
  when (In `elem` evts) $ do
    worker <- liftIO $ getAvailableWorker state
    if isNothing worker
      -- scale up if no available workers
      then do
        puts state "HIGH LOAD, SCALING UP"
        replicateM_ load_response_delta $
          installWorker state =<< idGen
      -- else, route client request to available worker
      else 
        let w = $fromJust worker
        in if (w^.status) == Unregistered
          then return ()
          else do
            [clientAddr, req] <- receiveMulti frontend
            -- Invariant: if the worker is Idle, it has a valid Network ID
            sendMulti backend $ fromList [ $fromJust (w^.networkId)
                                         , empty, clientAddr
                                         , empty, req
                                         ]

--------------------------------------------------------------------------------
-- Process messages


processMessage :: MVar State -> KernelRequest -> ZMQ z KernelReply
processMessage state req = case req of 
  EvaluateRequest scope _ sid code -> 
    EvaluateReply <$> liftIO (runBlock state code scope sid)
  ClearRequest sid -> 
    liftIO (clear state sid) >> return GenericSuccessReply
  HaltMessageRequest mid -> 
    haltMessage state mid >> return GenericSuccessReply
  GetStatusRequest mid -> 
    liftIO $ getMessageStatus state mid

-- | When the client pokes us for the status of a MessageId, we see if any 
-- worker is busy on that message. If so, we send [clientAddr, stillUpdating]
-- to the frontend router, which will send [stillUpdating] to the client at
-- clientAddr. Else, we do nothing.
getMessageStatus :: MVar State -> MessageId -> IO KernelReply
getMessageStatus st mid = do 
  putsTimed st $ "POKE REQUEST " ++ (T.unpack mid)
  state <- readMVar st
  let ws = M.filter (\w -> w^.status == Busy (Just mid)) (state^.workers)
  return $ if null ws 
    then GenericErrorReply "not working on that message"
    else StillProcessingReply

-- | Halt work on a message. Kills all workers working on a particular message, 
-- and replaces them with new ones.
haltMessage :: MVar State -> MessageId -> ZMQ z ()
haltMessage state mid = do
  theWorkers <- view workers <$> liftIO (readMVar state)
  let isBusy w = (w^.status) == (Busy (Just mid)) 
  let myWorkers = M.elems $ M.filter isBusy theWorkers
  forM_ myWorkers $ \worker -> do
    installWorker state (worker^.workerId)
    liftIO $ Async.cancel (worker^.thread)

