{-# LANGUAGE OverloadedStrings, QuasiQuotes, DataKinds #-}

module Main where
 
import AS.Config.Settings as S

import AS.Types.Messages
import AS.Types.Network

import AS.Clients()
import AS.Logging
import AS.Window
import AS.Util (sendMessage)
import AS.Config.Paths
import AS.DB.API as DB
import AS.DB.Graph as G
import AS.DB.Internal as DI
import AS.Users as US
import AS.Handlers.Misc (handleImportBinary)
import AS.Kernels.Python.Eval as KP

import Prelude
import System.Environment (getArgs)

import Control.Exception
import Control.Monad (forever, when)
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)

import Data.Aeson hiding (Success)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as L

-- often want to use these while debugging
-- import Text.Read (readMaybe)
-- import Text.ParserCombinators.Parsec (parse)

import qualified Network.WebSockets as WS
import qualified Database.Redis as R

-- EitherT
import Control.Monad.Trans.Either

import Language.R.Instance as R
import Language.R.QQ

-------------------------------------------------------------------------------------------------------------------------
-- Main

main :: IO ()
main = R.withEmbeddedR R.defaultConfig $ do
  -- initializations
  putStrLn "STARTING APP"
  (conn, ports, states) <- initApp
  G.recompute conn
  putStrLn "RECOMPUTED DAG"

  if isDebug -- set in Settings.hs
    then initDebug conn (head states)
    else return ()
  putStrLn $ "server started on ports " ++ (show ports)
  mapM_ (\(port, state) -> WS.runServer S.wsAddress port $ application state) (zip ports states)
  putStrLn $ "DONE WITH MAIN"

initApp :: IO (R.Connection, [Port], [MVar ServerState])
initApp = do
  runEitherT $ KP.evaluate "" "\'test!\'" -- force load C python sources so that first eval isn't slow

  -- init R
  R.runRegion $ do
    -- the app needs sudo to install packages.
    [r|library("rjson")|]
    [r|library("ggplot2")|]
    return ()
  -- init state
  conn <- R.connect DI.cInfo
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

-- |  for debugging. Only called if isDebug is true.
-- initDebug :: R.Connection -> MVar ServerState -> IO ()
-- initDebug conn state = 
initDebug :: R.Connection -> MVar ServerState -> IO ()
initDebug = const $ const $ return ()

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending -- initialize connection
  msg <- WS.receiveData conn -- waits until it receives data
  if (isDebug && shouldPreprocess) 
    then preprocess conn state
    else handleFirstMessage state conn msg

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

shouldPreprocess :: Bool
shouldPreprocess = False

-- | For debugging purposes. Reads in a list of ServerMessages from a file and processes them, as though
-- sent from a frontend. 
preprocess :: WS.Connection -> MVar ServerState -> IO () 
preprocess conn state = do
  -- prepare the preprocessing
  logDir <- getServerLogDir
  fileContents <- Prelude.readFile (logDir ++ "client_messages")
  let fileLinesWithNumbers = zip (L.lines fileContents) [1..]
      nonemptyNumberedFileLines =  filter (\(l, _) -> (l /= "") && (head l) /= '#') fileLinesWithNumbers

  mapM_ (\[(msg,i), (sid, _), (uid, _)] -> do 
    putStrLn ("PROCESSING LINE " ++ (show i) ++ ": " ++ msg ++ "\n" ++ sid ++ "\n" ++ uid)
    let win = Window (T.pack sid) (-1,-1) (-1,-1)
        cid = T.pack uid
        mockUc = UserClient cid conn win (T.pack "")
    curState <- readMVar state
    when (isNothing $ US.getUserByClientId cid curState) $ liftIO $ modifyMVar_ state (\s -> return $ addClient mockUc s)
    processMessage mockUc state (read msg)
    putStrLn "\n\n\n\nFINISHED PROCESSING MESSAGE\n\n\n\n") (chunksOf 3 nonemptyNumberedFileLines)
  putStrLn "\n\nFinished preprocessing."

-- too lazy to import from Data.List.Split
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
  | n > 0 = (take n l) : (chunksOf n (drop n l))
  | otherwise = error "Negative n"


initClient :: (Client c) => c -> MVar ServerState -> IO ()
initClient client state = do
  liftIO $ modifyMVar_ state (\s -> return $ addClient client s) -- add client to state
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

handleRuntimeException :: ASUserClient -> MVar ServerState -> SomeException -> IO ()
handleRuntimeException user state e = do
  let logMsg = "Runtime error caught: " ++ (show e)
  putStrLn logMsg
  logError logMsg (userCommitSource user)
  port <- appPort <$> readMVar state
  purgeZombies state
  WS.runServer S.wsAddress port $ application state

-- | Sometimes, onDisconnect gets interrupted. (Not sure exactly what.) At any rate, 
-- when this happens, a client that's disconnected is still stored in the state. 
-- This function makes sure they get removed from the state. Unsure if this is actually
-- a robust solution. 
purgeZombies :: MVar ServerState -> IO ()
purgeZombies state = do 
  ucs <- userClients <$> readMVar state
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
  dbConnection <- fmap dbConn (readMVar state) -- state stores connection to db; pull it out
  isPermissible <- DB.isPermissibleMessage (ownerName client) dbConnection message
  if (isPermissible || isDebug)
    then handleServerMessage client state message
    else sendMessage (failureMessage "Insufficient permissions") (conn client)

onDisconnect :: (Client c) => c -> MVar ServerState -> IO ()
onDisconnect user state = do
  printWithTime "Client disconnected"
  liftIO $ modifyMVar_ state (\s -> return $ removeClient user s) -- remove client from server
