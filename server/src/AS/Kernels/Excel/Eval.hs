module AS.Kernels.Excel.Eval where

import AS.Types.Core
import AS.Types.Excel

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

evaluate :: String -> EitherTExec ASValue
evaluate str = return NoValue -- TODO