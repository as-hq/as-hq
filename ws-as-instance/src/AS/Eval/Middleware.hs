module AS.Eval.Middleware where

import Prelude
import Data.List
import AS.Types
import AS.Eval as R
import AS.Util

{-
    Middlewares take a message (cells, etc) pushed to server, and process them before handing them off (to eval, etc)
    Here we apply a stack of middlewares
-}

-- | This is middleware for evaluation; we take a cell recieved with the "Evaluate" action tag and preprocess it
evalMiddleware :: ASCell -> IO ASCell
evalMiddleware cell = 
    evalInitExcel cell >>= 
        evalConnector >>=
            return

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Middlewares


evalInitExcel :: ASCell -> IO ASCell
evalInitExcel (Cell loc xp@(Expression rawXp Excel) val ts) = do
    newXp <- R.evalExcel xp
    return $ Cell loc newXp val ts
evalInitExcel cell = return cell

evalConnector :: ASCell -> IO ASCell
evalConnector cell = return cell -- TODO