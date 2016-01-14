{-# LANGUAGE TemplateHaskell #-}
module AS.Config.Settings where

import Prelude()
import AS.Prelude
import AS.Config.Paths
import qualified AS.Config.Internal as I
import AS.Types.Network
import AS.Types.Cell

imagesStaticPath = "static/images/"
-- For debugging purposes, the app behaves differently at various points in the code.
-- if you see (if isDebug), behavior forks.
isDebug :: Bool
isDebug = True

shouldWritetoConsole :: Bool
shouldWritetoConsole = $(I.getPrintSetting)

largeSearchBound :: Int
largeSearchBound = 1000

headerLangs :: [ASLanguage]
headerLangs = [Python, R] 

getSettings :: IO AppSettings
getSettings = I.getSettings

heartbeat_interval :: Milliseconds
heartbeat_interval = 1000