module AS.Kernels.R.Server where

import AS.Prelude

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
import Data.Maybe (isNothing)
import Data.List.NonEmpty (fromList)
import System.ZMQ4.Monadic
import Safe (headMay)
import Control.Lens
import Control.Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Monad
import Control.Monad.IO.Class

import qualified Language.R.Instance as R
import Language.R.QQ

url_workers = "inproc://myworkers"
load_response_delta = 5
empty = ""

puts x = liftIO $ putStrLn x 

idGen :: MonadIO m => m B.ByteString
idGen = pack <$> liftIO getUniqueId

-- | main server loop. REQUIRES THAT R HAS ALREADY BEEN INITIALIZED.
runServer :: Addr -> Int -> IO ()
runServer url_clients numInitialWorkers = do
  shell <- newShell
  state <- newMVar (initialState shell)
  initialize state

  runZMQ $ do
    puts "Starting workers..."
   
    replicateM_ numInitialWorkers $ 
      installWorker state =<< idGen

    puts $ "Server listening on " ++ url_clients

    frontend <- socket Router
    bind frontend url_clients
    backend <- socket Router
    bind backend url_workers

    forever $ do
      [evtsB, evtsF] <- poll (-1) [Sock backend [In] Nothing, Sock frontend [In] Nothing]
      processBackend state (backend, frontend) evtsB
      processFrontend state (frontend, backend) evtsF

-- | initialize namespaces with header values
initialize :: MVar State -> IO ()
initialize state = do
  dbConn <- connectRedis
  headers <- getAllHeaders dbConn R
  forM_ headers $ \header -> 
    let code = header^.evalHeaderExpr
        sid = header^.evalHeaderSheetId
    in runBlock state code Header sid

-- | create and install a worker. Its status is 'Unregistered' until it sends a registration message.
installWorker :: MVar State -> WorkerId -> ZMQ z ()
installWorker state wid = do
  lock <- liftIO $ newEmptyMVar
  thread <- async $ liftIO (takeMVar lock) >>= \_ -> runWorker state wid
  let worker = Worker wid Nothing thread Unregistered
  liftIO $ do
    modifyMVar_' state $ 
      return . (& workers %~ M.insert wid worker) . (& numWorkers %~ (1+))
    putMVar lock ()

-- | Upon reception of a worker registration message, update its status and network id.
registerWorker :: MVar State -> WorkerId -> NetworkId -> IO ()
registerWorker state wid netid = 
  modifyMVar_' state $ 
    return . (& workers %~ M.update 
      (Just . (& networkId .~ Just netid)
            . (& status .~ Idle))
      wid)

-- | Get a worker that is not engaged.
getAvailableWorker :: MVar State -> IO (Maybe Worker)
getAvailableWorker state = readMVar state >>= (\s ->
  let isIdle w = (w^.status) `elem` [Idle, Unregistered]
  in return $ headMay . M.elems $ M.filter isIdle (s^.workers))

-- | Handle worker activity on backend
processBackend :: (Receiver r, Sender s) => MVar State -> (Socket z r, Socket z s) -> [Event] -> ZMQ z ()
processBackend state (backend, frontend) evts
  | In `elem` evts = do
    msg <- receiveMulti backend
    if (msg !! 2 == "READY") 
      -- register the network id of a ready worker
      then liftIO $ do
        let netid = msg !! 0
        let wid = msg !! 4
        putStrLn $ "WORKER REGISTERED: " ++ show wid
        registerWorker state wid netid
      -- route reply back to client
      else do
        let clientAddr = msg !! 2
        let reply = msg !! 4
        liftIO $ getTime >>= \t -> puts ("==== SENT BACK REPLY ==== " ++  t)
        sendMulti frontend $ fromList [clientAddr, "", reply]
  | otherwise = return ()

-- | Handle client activity on frontend
processFrontend :: (Receiver r, Sender s) => MVar State -> (Socket z r, Socket z s) -> [Event] -> ZMQ z ()
processFrontend  state (frontend, backend) evts
  | In `elem` evts = do
    worker <- liftIO $ getAvailableWorker state
    if isNothing worker
      -- scale up if no available workers
      then do
        puts "HIGH LOAD, SCALING UP"
        replicateM_ load_response_delta $
          installWorker state =<< idGen
      -- else, route client request to worker
      else 
        let w = $fromJust worker
        in if (w^.status) == Unregistered
          then return ()
          else do
            [clientAddr, "", req] <- receiveMulti frontend
            sendMulti backend $ fromList [$fromJust (w^.networkId), "", clientAddr, "", req]
            --                            ^
            -- invariant: if 'Idle', the worker has a valid Network ID. 
            -- (all workers are registered)
  | otherwise = return ()

setWorkerStatus :: MVar State -> WorkerId -> WorkerStatus -> IO ()
setWorkerStatus state wid s = modifyMVar_' state $ 
  return . (& workers %~ M.update 
    (Just . (& status .~ s))
    wid)

-- | worker routine
runWorker :: MVar State -> WorkerId -> ZMQ z ()
runWorker state wid = do
  sock <- socket Req
  setIdentity (restrict wid) sock
  connect sock url_workers
  sendMulti sock $ fromList ["READY", "", wid]

  forever $ do
    -- blocks here until the worker is registered 
    -- (due to invariant: unregistered worker never assigned work)
    [clientAddr, "", msg] <- receiveMulti sock
    liftIO $ getTime >>= \t -> puts ("==== WORKER RECEIVED MESSAGE ==== " ++  t)

    let req = Serial.decode msg
    let mid = case req of 
              Right (EvaluateRequest _ m _ _) -> Just m
              _ -> Nothing

    -- set status to busy while doing work.
    liftIO $ setWorkerStatus state wid (Busy mid)

    rep <- case req of 
      Right myReq -> processMessage state myReq
      Left err    -> return $ GenericErrorReply err
    sendMulti sock $ fromList [clientAddr, "", Serial.encode rep]

    -- reset idle status. 
    liftIO $ setWorkerStatus state wid Idle

processMessage :: MVar State -> KernelRequest -> ZMQ z KernelReply
processMessage state req = case req of 
  EvaluateRequest scope _ sid code  -> EvaluateReply <$> liftIO (runBlock state code scope sid)
  ClearRequest sid                  -> liftIO (clear state sid) >> return GenericSuccessReply
  HaltMessageRequest mid            -> haltMessage state mid >> return GenericSuccessReply

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
