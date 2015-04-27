module AS.Constants where

import Prelude
import qualified Import(FilePath)

py_eval_path = "/home/hal/code/alphasheets/as-instance/as-py-eval/" :: String
py_run_path = py_eval_path ++ "run/" :: Import.FilePath
py_eval_file = py_eval_path ++ "eval.py" :: Import.FilePath
py_temp_run_file = "temp.py" :: Import.FilePath
py_template_file = "template.py" :: Import.FilePath