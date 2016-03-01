module AS.Config.Internal where

import Data.Aeson hiding (Success)
import System.Directory
import System.FilePath.Posix
import Control.Exception
import Control.Lens
import Language.Haskell.TH
import qualified Data.ByteString.Lazy.Char8 as B

import Prelude()
import AS.Prelude
import AS.Types.Network

-- This needs to be in a module by itself, because TemplateHaskell

-------------------------------------------------------------------------------------------------------------------------
-- compile and run-time settings 

getPrintSetting :: Q Exp
getPrintSetting = do
  settings <- runIO getSettings
  return . ConE . mkName . show $ settings^.shouldWriteToConsole

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
      case eitherDecode env of 
        Right settings -> do 
          putStrLn $ "using settings from Environment.json: "
          putStrLn $ B.unpack env
          putStrLn $ "Settings: " ++ show settings
          return settings
        Left err -> $error $ "couldn't decode environment file, because: " ++ err
    handleException :: SomeException -> IO AppSettings
    handleException e = $error $ "decoding Environment failed with error: " ++ show e 

--readConfiguration :: String -> Q Exp
--readConfiguration filepath = 

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