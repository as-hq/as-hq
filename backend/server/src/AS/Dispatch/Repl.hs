-- Commenting out until we actually reuse this. (Alex 12/28)
module AS.Dispatch.Repl where

-- import AS.Prelude

-- import AS.Types.Cell
-- import AS.Types.Messages 
-- import AS.Types.Eval

-- import AS.Eval.Core as R (evaluateLanguageRepl)

-- import AS.Util

-- -- EitherT
-- import Control.Monad.Trans.Class
-- import Control.Monad.Trans.Either

-- -- this file is for future kernel-based repl methods

-- -- #needsrefactor should return the value, not the message
-- runReplDispatch :: ASSheetId -> ASExpression -> IO ClientMessage
-- runReplDispatch sid xp = do
--     let lang = language xp
--     -- "" for header right now because the repl is out of commission for now, so I'm not going
--     -- to fix it. (Alex 12/4)
--     val <- runEitherT $ R.evaluateLanguageRepl "" xp 
--     return $ case val of 
--         Left e -> ClientMessage EvaluateHeader (Failure $ generateErrorMessage e) (PayloadValue (CellValue $ execErrorToValueError e) lang)
--         Right v -> ClientMessage EvaluateHeader Success (PayloadValue v lang)
