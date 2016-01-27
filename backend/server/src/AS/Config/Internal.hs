module AS.Config.Internal where

import Prelude()
import AS.Prelude

import AS.Types.Network
import AS.Config.Environment

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson hiding (Success)

import System.Directory
import System.FilePath.Posix

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
      mainDir <- getCurrentDirectory
      env <- B.readFile $ mainDir </> env_dir
      case (eitherDecode env) of 
        Right settings -> putStrLn ("using settings from Environment.json: " ++ show env) >> return settings
        Left err -> $error $ "couldn't decode environment file, because: " ++ err
    handleException :: SomeException -> IO AppSettings
    handleException _ = putStrLn "decoding Environment failed, falling back on defaults" >> return defaultSettings

-------------------------------------------------------------------------------------------------------------------------
-- filepaths 

images_dir = "static/images/"
eval_dir   = "eval_files/"
env_dir    = ".." </> "Environment.json"
log_dir    = "logs/"
static_dir = "static/"

-- This function is for embedding filepaths at compile-time into Config/Settings.hs.
-- This means that, as always, you should compile at the root stack project directory,
-- and run the app on the same machine as it was compiled on.
getAppendedPath :: String -> Q Exp
getAppendedPath dir = do
  mainDir <- runIO getCurrentDirectory
  return . LitE . StringL $ mainDir </> dir