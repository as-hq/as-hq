{-# LANGUAGE TemplateHaskell #-}
module AS.Config.Settings where

import Prelude()
import AS.Prelude
import qualified AS.Config.Internal as I
import AS.Types.Network
import AS.Types.Cell

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

process_message_timeout :: Seconds
process_message_timeout = 3

main_dir :: String
main_dir = $(I.getAppendedPath "")

static_dir :: String 
static_dir = $(I.getAppendedPath I.static_dir)

images_dir :: String
images_dir = $(I.getAppendedPath I.images_dir)

eval_dir :: String
eval_dir = $(I.getAppendedPath I.eval_dir)

env_path :: String
env_path = $(I.getAppendedPath I.env_dir)

log_dir :: String
log_dir = $(I.getServerLogDir)