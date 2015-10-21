module AS.Config.Paths where

import Prelude
import System.Directory
import System.FilePath.Posix

import AS.Util

py_eval_path = "server/as-py-eval/"
py_libs_path = "asl/as-libs/py/"
py_template_file = "template.py"
py_eval_file = "eval.py"
py_run_file = "run.py"


getImagesPath :: IO String
getImagesPath = do
  mainDir <- getCurrentDirectory
  return $ mainDir </> "static/images/"

getEvalPath :: IO String
getEvalPath = do
  mainDir <- getCurrentDirectory
  return $ mainDir </> "eval_files/"
