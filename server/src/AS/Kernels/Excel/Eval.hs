module AS.Kernels.Excel.Eval where

import AS.Types.Core
import AS.Types.Excel

evaluate :: String -> IO (Either ASExecError ASValue)
evaluate str = return $ Right NoValue -- TODO