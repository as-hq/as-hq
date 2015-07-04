module Handler.EvalRepl where

import Import
import AS.HandlerLibrary
import AS.Types hiding (error)
import qualified AS.Eval.Lang as LA
import qualified AS.Eval as R


optionsEvalReplR :: Handler RepPlain
optionsEvalReplR = do
    addCorsHeaders
    return $ RepPlain $ toContent ("" :: Text)

postEvalReplR :: Handler Value
postEvalReplR = interactHandlerJson process
    where
        process :: ASExpression -> Handler ASValue
        process = R.evalCodeRepl