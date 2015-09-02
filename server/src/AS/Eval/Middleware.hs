module AS.Eval.Middleware where

import Prelude
import Data.List
import AS.Types
import AS.Eval as R
import AS.Util as U

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

-- | Change the excel expression to a python one and also possibly add a volatile tag
evalInitExcel :: ASCell -> IO ASCell
evalInitExcel c@(Cell loc xp@(Expression rawXp Excel) val ts) = do
    (newXp,isVolatile) <- R.evalExcel xp 
    case isVolatile of
      True -> case (U.hasVolatileTag c) of 
        True -> return $ Cell loc newXp val ts
        False -> return $ Cell loc newXp val (Volatile:ts)
      False -> return $ Cell loc newXp val ts
    return $ Cell loc newXp val ts
evalInitExcel cell = return cell

evalConnector :: ASCell -> IO ASCell
evalConnector cell = return cell -- TODO