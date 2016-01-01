module AS.Config.Settings where

import Prelude
import AS.Types.Network
import AS.Types.Cell

wsAddress = "0.0.0.0"
wsDefaultPort= 5000 :: Port
graphDbHost = "tcp://localhost:5555"
imagesStaticPath = "static/images/"
-- For debugging purposes, the app behaves differently at various points in the code.
-- if you see (if isDebug), behavior forks.
isDebug = True
isBenchmark = True

largeSearchBound :: Int
largeSearchBound = 1000

headerLangs = [Python, R] 