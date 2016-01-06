{-# LANGUAGE TemplateHaskell #-}
module AS.Config.Settings where

import Prelude
import AS.Config.Paths
import qualified AS.Config.Internal as I
import AS.Types.Network
import AS.Types.Cell
import AS.TH

imagesStaticPath = "static/images/"
-- For debugging purposes, the app behaves differently at various points in the code.
-- if you see (if isDebug), behavior forks.
isDebug :: Bool
isDebug = True

shouldWritetoConsole :: Bool
shouldWritetoConsole = $getPrintSetting

largeSearchBound :: Int
largeSearchBound = 1000

headerLangs :: [ASLanguage]
headerLangs = [Python, R] 

getSettings :: IO AppSettings
getSettings = I.getSettings