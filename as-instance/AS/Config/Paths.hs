module AS.Config.Paths where

import Import hiding ((</>))
import System.IO 
import System.Directory(getCurrentDirectory)
import System.FilePath.Posix
import Control.Applicative   

-- basepath = do
-- 	path <- liftIO 
-- 	return path
-- py_eval_path = basepath ++ "as-instance/as-py-eval/"
-- -- TODO fix libs path
-- py_libs_path = "/home/hal/code/alphasheets/as-libs/py/"
-- py_run_path = py_eval_path ++ "run/"
-- py_template_file = py_eval_path ++ "template.py"
-- py_eval_file = py_eval_path ++ "eval.py"
-- py_run_file = py_run_path ++ "temp.py"

py_eval_path = "as-instance/as-py-eval/"
py_libs_path = "alphasheets/as-libs/py/"
py_template_file = "template.py"
py_eval_file = "eval.py"
py_run_file = "temp.py"

-- getPath :: String
-- getPath = do
--   basepath <- liftIO $ getCurrentDirectory
--   return basepath

-- getEvalPath :: String
-- getEvalPath = do
--   basepath <- liftIO $ getCurrentDirectory
--   return $ (takeDirectory basepath) </> "Eval" </> "run"

getEvalPath = "/home/hal/code/alphasheets/as-instance/AS/Eval/run/"
