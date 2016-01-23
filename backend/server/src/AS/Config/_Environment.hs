-- This is a file with settings that may be local to your development environment, e.g., 
-- whether to log to the console. (Don't want this in production.) 
-- 
-- We're storing this information in an environemnt file (Environment.hs), which
-- is in the .gitignore. _Environment.hs exists as a template for creating Environment.hs, 
-- for convenience (since Environment.hs is in the .gitignore)._Environment.hs is NOT
-- actually imported by any other modules, and is not in the .cabal. 

module AS.Config.Environment where

import AS.Types.Network

defaultSettings :: AppSettings
defaultSettings = AppSettings  { _backendWsAddress = "0.0.0.0"
                                , _backendWsPort = 5000
                                , _graphDbAddress = "tcp://localhost:5555"
                                , _pyKernelAddress = "tcp://localhost:20000"
                                , _redisPort = 6379
                                , _redisHost = "localhost"
                                , _shouldPrint = False
                                , _shouldWriteToSlack = True} -- Should only be True in production environment