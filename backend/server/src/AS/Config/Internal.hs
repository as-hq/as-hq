module AS.Config.Internal where

import Prelude()
import AS.Prelude

import AS.Types.Network
import AS.Config.Paths
import AS.Config.Environment

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson hiding (Success)

import Control.Exception
import Control.Lens
import Language.Haskell.TH
-- needs to be in a module by itself, because templatehaskell... 

-------------------------------------------------------------------------------------------------------------------------
-- compile- and run-time settings 

getPrintSetting :: Q Exp
getPrintSetting = do
  settings <- runIO getSettings
  return . ConE . mkName . show $ settings^.shouldPrint

getShouldWriteToSlack :: Q Exp
getShouldWriteToSlack = do
  settings <- runIO getSettings
  return . ConE . mkName . show $ settings^.shouldWriteToSlack

-- currently always fails, and always reads from defaultSettings (Alex 1/22)
getSettings :: IO AppSettings
getSettings = catch readEnvironment handleException
  where 
    readEnvironment = do
      env <- B.readFile =<< getEnvironmentPath
      case (eitherDecode env) of 
        Right settings -> putStrLn ("using settings from Environment.json: " ++ show env) >> return settings
        Left err -> $error $ "couldn't decode environment file, because: " ++ err
    handleException :: SomeException -> IO AppSettings
    handleException _ = putStrLn "decoding Environment failed, falling back on defaults" >> return defaultSettings