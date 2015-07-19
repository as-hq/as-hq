module AS.Config.Paths where

import Import hiding ((</>))
import System.IO 
import System.Directory(getCurrentDirectory)
import System.FilePath.Posix
import Control.Applicative   

py_eval_path = "as-instance/as-py-eval/"
py_libs_path = "asl/as-libs/py/"
py_template_file = "template.py"
py_eval_file = "eval.py"
py_run_file = "temp.py"

getEvalPath = "/home/anand/Development/as/asl-demo/as-instance/AS/Eval/run/"
