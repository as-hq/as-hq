module Handler.EvalRepl where

import Import
import AS.Eval.Py(evalPy)

postEvalReplR :: String -> Handler Value
postEvalReplR cmd = do
	result <- evalPy cmd 
	returnJson $ show $ liftIO result
