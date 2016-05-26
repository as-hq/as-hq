module Main where

import Lib hiding (status)

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BC

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Control.Concurrent.MVar

import Network.Wai.Handler.Warp (setTimeout, setPort, defaultSettings)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Cors
import Network.HTTP.Types (status403, status400, status200)
import Web.Scotty 

router_port     = 11000
server_timeout  = 3600
admin_password :: String
admin_password  = "poiuytrewq"

config :: Options
config = Options { verbose = 1
                 , settings = setTimeout server_timeout . setPort router_port $ defaultSettings }

main :: IO ()
main = do
  puts "Destroying old instances..."
  puts =<< runCmd "docker stop $(docker ps -a -q)"
  puts =<< runCmd "docker rm $(docker ps -a -q)"

  puts "Creating state and instances."
  state <- newState
  forkIO $ runStatusUpdater state
  forkIO $ runInstancePool state
  replicateM_ num_initial_instances $ installInstance state 
  puts $ "Created " ++ show num_initial_instances ++ " instances"

  puts "Starting HTTP Server."
  scottyOpts config $ do
    middleware logStdoutDev
    middleware simpleCors

    get "/" $ do
      puts "received GET, producing available instance"
      json =<< liftIO (produceAvailableInstance state)
      puts "responded to GET"

    post "/" $ do
      body <- jsonData :: ActionM (M.Map String String)
      puts "received POST"
      puts $ show body

      case ("admin_password" `M.lookup` body) of
        Just pwd -> if (pwd == admin_password) 
          then case ("action" `M.lookup` body) of 

            Just "create" -> do
              liftIO $ installInstance state 
              status status200

            Just "destroy" -> do
              s <- liftIO $ readMVar state
              let iname = BC.pack $ body M.! "name"
              if iname `M.member` (s^.instances)
                then liftIO (destroyInstance state iname) >> status status200
                else puts "CANNOT DESTROY INSTANCE BECAUSE NOT FOUND" >> status status400

            Just "get_instance" -> do
              let iname = BC.pack $ body M.! "name"
              i <- liftIO $ getInstance state iname
              case i of 
                Just inst -> json inst
                Nothing -> do
                  puts "CANNOT FIND INSTANCE:"
                  puts $ show iname
                  status status400

            Just "get_all_instances" -> json =<< liftIO (getAllInstances state)

            Just "destroy_all" -> liftIO (destroyAll state) >> status status200

            Nothing -> do
              puts "RECEIVED INVALID POST: "
              puts $ show body
              status status400

          else do 
            puts "NOT AUTHENTICATED"
            status status403

        Nothing -> do
          puts "RECEIVED POST without authentication"
          status status403