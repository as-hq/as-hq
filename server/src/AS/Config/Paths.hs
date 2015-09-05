module AS.Config.Paths where

import Prelude
import System.Directory
import Filesystem.Path

py_eval_path = "server/as-py-eval/"
py_libs_path = "asl/as-libs/py/"
py_template_file = "template.py"
py_eval_file = "eval.py"
py_run_file = "temp.py"

-- getEvalPath = "/home/anand/Development/as/backend/server/src/AS/Eval/run/"
-- ::TODO:: something like this; couldn't get it to compile, since my cabal is broken. 
--          THIS CODE MIGHT NOT BE CORRECT. 
getEvalPath = do
    configDir <- getCurrentDirectory
    let asDir = parent dir 
    return $ asDir </> "Eval" </> "run/"