module AS.Eval.Middleware where

import Prelude
import Data.List
import Database.Redis (Connection)

import AS.Types.Core
import AS.Eval.Core as R
import AS.Util as U

{-
    Middlewares take a message (cells, etc) pushed to server, and process them before handing them off (to eval, etc)
    Here we apply a stack of middlewares
-}

-- | This is middleware for evaluation; we take a cell recieved with the "Evaluate" action tag and preprocess it
evalMiddleware :: ASCell -> IO ASCell
evalMiddleware cell = return cell

----------------------------------------------------------------------------------------------------------------------------------------------
-- Middlewares

-- | Change the excel expression to a python one and also possibly add a volatile tag
--evalInitExcel :: ASCell -> IO ASCell
--evalInitExcel c@(Cell loc xp@(Expression rawXp Excel) val ts) = do
--    initResult <- R.evalExcel xp
--    return $ case initResult of 
--        (Left valueE) -> Cell loc xp valueE ts
--        (Right (newXp, isVolatile)) -> case isVolatile of
--            False -> Cell loc newXp val ts
--            True -> case (U.hasVolatileTag c) of 
--                True -> Cell loc newXp val ts
--                False -> Cell loc newXp val (Volatile:ts)
--evalInitExcel cell = return cell

evalConnector :: ASCell -> IO ASCell
evalConnector cell = return cell -- TODO