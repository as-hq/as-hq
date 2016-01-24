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

--py_eval_path = "server/as-py-eval/"
--py_libs_path = "asl/as-libs/py/"
--py_template_file = "template.py"
--py_eval_file = "eval.py"
--py_run_file = "run.py"

images_dir = "static/images/"
eval_dir   = "eval_files/"
env_dir    = ".." </> "Environment.json"
log_dir    = "logs/"

getAppendedPath :: String -> Q Exp
getAppendedPath dir = do
  mainDir <- runIO getCurrentDirectory
  return . LitE . StringL $ mainDir </> dir

getServerLogDir :: Q Exp
getServerLogDir = do
  mainDir <- runIO getCurrentDirectory
  let logDir = mainDir </> log_dir
  runIO $ createDirectoryIfMissing True logDir
  return . LitE . StringL $ logDir