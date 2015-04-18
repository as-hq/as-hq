module Handler.EvalRepl where

import Import
import AS.eval.py (evalPy)

postEvalReplR :: String -> Handler Value
postEvalReplR cmd = do
	result <- evalPy cmd 
	returnJson $ show $ liftIO result
