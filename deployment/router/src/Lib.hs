{-# LANGUAGE FlexibleContexts #-}

module Lib where

import Data.Maybe (listToMaybe)
import qualified Data.Map as M
import Data.List (intercalate, sort, sortBy)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack, unpack)

import Text.Parsec hiding (State)
import Text.Parsec.String

import System.IO
import GHC.Generics
import Data.Aeson
import Control.Lens hiding ((.=))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad
import Control.Concurrent
import Web.Hashids
import System.Process
import Debug.Trace

num_initial_instances :: Int
num_initial_instances = 30
min_percent_free      = 0.2
load_balance_interval = 5 -- seconds

--------------------------------------------------------------------------------------------------------------
-- Types

type InstanceName = B.ByteString
type Port = Int

data Instance = Instance 
  { _backendPort :: Port
  , _filePort :: Port
  , _staticPort :: Port
  , _name :: InstanceName
  , _status :: Maybe InstanceStatus
  , _isIdle :: Bool}
  deriving (Show, Generic)

data State = State
  { _lastCreatedInstance :: Maybe Instance
  , _instances :: M.Map InstanceName Instance}
  deriving (Show)

data InstanceStatus = InstanceStatus 
  { _cpuPercent :: Double
  , _memPercent :: Double}
  deriving (Show, Generic)

makeLenses ''Instance
makeLenses ''InstanceStatus
makeLenses ''State

instance ToJSON Instance where
  toJSON i = object 
    [ "backendPort" .= (i^.backendPort)
    , "filePort" .= (i^.filePort)
    , "staticPort" .= (i^.staticPort)
    , "name" .= (unpack $ i^.name)
    , "cpuPercent" .= (view cpuPercent <$> i^.status)
    , "memPercent" .= (view memPercent <$> i^.status)
    , "isIdle" .= (i^.isIdle)
    ]

--------------------------------------------------------------------------------------------------------------
-- Settings

record_db = True

--------------------------------------------------------------------------------------------------------------
-- Exposed

newState :: IO (MVar State)
newState = newMVar $ State Nothing M.empty

getAllInstances :: MVar State -> IO [Instance]
getAllInstances state = M.elems . view instances <$> readMVar state

getInstance :: MVar State -> InstanceName -> IO (Maybe Instance)
getInstance state iname = M.lookup iname . view instances <$> readMVar state 

-- | uses immediately-returning spinup function
installInstance :: MVar State -> IO Instance
installInstance state = do
  st <- readMVar state
  let i = maybe defaultInstance incrementPorts (st^.lastCreatedInstance)
  i `seq` return ()

  -- insert in state
  modifyMVar_ state $ return
    . (& instances %~ M.insert (i^.name) i)
    . (& lastCreatedInstance .~ Just i)

  -- spinup
  spinup i
  return i

destroyInstance :: MVar State -> InstanceName -> IO ()
destroyInstance state iname = modifyMVar_ state $ \s -> 
  case M.lookup iname (s^.instances) of 
    Just i -> spindown i >> return (s & instances %~ M.delete iname) 
    Nothing -> print "COULD NOT DESTROY NONEXISTENT INSTANCE" >> return s

destroyAll :: MVar State -> IO ()
destroyAll state = modifyMVar_ state $ \s -> do
  mapM_ spindown (M.elems $ s^.instances)
  return $ s & instances .~ M.empty

produceAvailableInstance :: MVar State -> IO Instance
produceAvailableInstance state = do
  st <- readMVar state
  puts "read state"
  i <- maybe (installInstance state)
             return
             (getAvailableInstance st)
  puts "got instance"
  setIdleStatus state (i^.name) False
  puts "set unidle"
  return i

--------------------------------------------------------------------------------------------------------------
-- Server loops

runStatusUpdater :: MVar State -> IO ()
runStatusUpdater state = forever $ do
  st <- readMVar state
  forM_ (M.keys $ st^.instances) $ \iname -> do
    stat <- getStatus iname
    -- ensure we are always providing valid instances
    running <- isRunning iname
    -- for now, do nothing if got null status
    let updater = if running 
                    then (Just . (& status .~ stat)) 
                    else trace ("Instance " ++ unpack iname ++ " got null status") `seq` (\_ -> Nothing)
    modifyMVar_ state $ return
      . (& instances %~ M.update updater iname)
  threadDelay 1000000

runInstancePool :: MVar State -> IO ()
runInstancePool state = forever $ do
  threadDelay (load_balance_interval * 1000000)
  st <- readMVar state
  puts "checking pool, current instances: "
  showInstances state
  let n = fromIntegral $ M.size $ st^.instances
  let k = fromIntegral $ freeInstanceCount st
  when (k / n < min_percent_free) $ do
    puts "High load, adding more instances"
    replicateM_ (round $ n * k / n)  $ installInstance state

--------------------------------------------------------------------------------------------------------------
-- Instance utils

defaultInstance :: Instance
defaultInstance = Instance 
  { _backendPort = 20000 
  , _filePort = 30000
  , _staticPort = 40000
  , _name = encodeList hContext [20000,30000,40000]
  , _status = Nothing
  , _isIdle = True
  }

incrementPorts :: Instance -> Instance 
incrementPorts i = 
    (& backendPort .~ b)
  . (& filePort .~ f)
  . (& staticPort .~ s)
  . (& name .~ n)
  $ i
  where
    b = view backendPort i + 1
    f = view filePort i + 1
    s = view staticPort i + 1
    n = encodeList hContext [b,f,s]

freeInstanceCount :: State -> Int
freeInstanceCount = length . idleInstances

-- | Produces the least recently spun-up idle instance, if there is one.
getAvailableInstance :: State -> Maybe Instance
getAvailableInstance = listToMaybe . sortBy minPort . idleInstances
  where 
    minPort i1 i2 
      | (i1^.backendPort) < (i2^.backendPort) = LT
      | (i1^.backendPort) > (i2^.backendPort) = GT
      | otherwise = EQ

idleInstances :: State -> [Instance]
idleInstances = M.elems . M.filter (view isIdle) . view instances

setIdleStatus :: MVar State -> InstanceName -> Bool -> IO ()
setIdleStatus state iname idle = 
  modifyMVar_ state $ 
    return . (& instances %~ M.update updater iname)
  where updater = Just . (& isIdle .~ idle)

--------------------------------------------------------------------------------------------------------------
-- Instance actions

-- | returns immediately
spinup :: Instance -> IO ()
spinup i = forkIO_ $
  runCmd_ spinupCmd >> puts ("spun up " ++ name') >> possiblyRecordDb
  where 
    spinupCmd = intercalate " "
      [ "docker run -d"
      , backendConfig
      , fileConfig
      , staticConfig
      , envConfig 
      , nameConfig 
      , "alphasheets/demo"
      ]
    possiblyRecordDb = when record_db $ do
      dir <- getDumpDir (i^.name)
      runCmd_ $ "echo " ++ dir ++ " >> ~/dumpdir.txt"
    backendConfig = mkPort (i^.backendPort) 5000
    fileConfig    = mkPort (i^.filePort) 9000
    staticConfig  = mkPort (i^.staticPort) 8000
    envConfig     = "-e NAME=" ++ name'
    nameConfig    = "--name=" ++ name'
    mkPort p1 p2  = "-p " ++ show p1 ++ ":" ++ show p2
    name'         = unpack $ i^.name

-- | returns immediately
spindown :: Instance -> IO ()
spindown i = forkIO_ $ 
  runCmd_ ("docker stop " ++ iname) >> runCmd_ ("docker rm " ++ iname)
  where iname = unpack $ i^.name

getStatus :: InstanceName -> IO (Maybe InstanceStatus)
getStatus iname = eitherToMaybe . parse statusP "" <$> str
  where 
    str = runCmd $ "docker stats --no-stream " ++ unpack iname
    eitherToMaybe (Right a) = Just a
    eitherToMaybe (Left e) = trace ("Status parse error: " ++ show e) `seq` Nothing

statusP :: Parser InstanceStatus
statusP = 
  let float     = (read <$> floatStr) :: Parser Double 
      floatStr  = many1 (oneOf "1234567890") +> string "." +> many1 (oneOf "1234567890")
      eatTill p = manyTill anyChar (try (lookAhead $ void p <|> eof)) 
      dontcare  = eatTill float
      surroundedBy r p = p >> r `endBy` p
  in do
    [cpuPer, _, _, memPer, _] <- float `surroundedBy` dontcare
    return $ InstanceStatus cpuPer memPer

getDumpDir :: InstanceName -> IO String
getDumpDir iname = runCmd cmd
  where cmd = "docker inspect --format='{{(index .Mounts 0).Source}}' " ++ unpack iname

isRunning :: InstanceName -> IO Bool
isRunning iname = do
  result <- runCmd $ "docker inspect --format='{{.State.Running}}' " ++ unpack iname
  return $ case (head result) of 
    't' -> True
    _ -> False 

showInstances :: (MonadIO m) => MVar State -> m ()
showInstances state = puts . show . sort . map (view backendPort) . M.elems . view instances =<< st
  where st = liftIO $ readMVar state

--------------------------------------------------------------------------------------------------------------
-- random shit

(+>) a b = a >>= (\r -> (r ++) <$> b)

hContext :: HashidsContext
hContext = hashidsSimple "uSaltyBro?"

forkIO_ = void . forkIO

runCmd_ = void . runCmd  

runCmd :: String -> IO String
runCmd cmd = do
  (_, stdout, _, process) <- runInteractiveCommand cmd
  result <- System.IO.hGetContents stdout
  foldr seq (waitForProcess process) result
  return result

fromRight :: Either a b -> b
fromRight (Right r) = r

puts :: (MonadIO m) => String -> m ()
puts x = liftIO $ putStrLn x