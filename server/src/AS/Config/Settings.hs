module AS.Config.Settings where

import Prelude
import AS.Types.Network
import AS.Types.Cell

imagesStaticPath = "static/images/"
-- For debugging purposes, the app behaves differently at various points in the code.
-- if you see (if isDebug), behavior forks.
isDebug = True
shouldWritetoConsole = False

largeSearchBound :: Int
largeSearchBound = 1000

headerLangs = [Python, R] 