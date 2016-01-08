module AS.Config.Environment where

import AS.Types.Network

defaultSettings :: AppSettings
defaultSettings = AppSettings  { _backendWsAddress = "0.0.0.0"
                                , _backendWsPort = 5000
                                , _graphDbAddress = "tcp://localhost:5555"
                                , _pyKernelAddress = "tcp://localhost:20000"
                                , _redisPort = 6379
                                , _shouldPrint = False}