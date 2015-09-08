module AS.Config.Settings where

import Prelude

frontend_url = "http://localhost:8080/app"
wsAddress = "0.0.0.0"
wsPort = 5000 :: Int
graphDbHost = "tcp://localhost:5555"
-- For debugging purposes, the app behaves differently at various points in the code.
-- if you see (if isDebug), behavior forks. 
isDebug = True