module AS.Config.Internal where

import AS.Types.Network
import AS.Config.Paths

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson hiding (Success)

import Prelude
import Control.Exception

-- needs to be in a module by itself, because templatehaskell... 

getSettings :: IO AppSettings
getSettings = catch readEnvironment handleException
  where 
    readEnvironment = do
      env <- B.readFile =<< getEnvironmentPath
      case (eitherDecode env) of 
        Right settings -> putStrLn "using settings from Environment.json" >> return settings
        Left err -> error $ "couldn't decode environment file, because: " ++ err
    handleException :: SomeException -> IO AppSettings
    handleException _ = putStrLn "decoding Environment failed, falling back on defaults" >> return defaultSettings

defaultSettings :: AppSettings
defaultSettings = AppSettings  { _backendWsAddress = "0.0.0.0"
                                , _backendWsPort = 5000
                                , _graphDbAddress = "tcp://localhost:5555"
                                , _pyKernelAddress = "tcp://localhost:20000"
                                , _redisPort = 6379
                                , _shouldPrint = True}