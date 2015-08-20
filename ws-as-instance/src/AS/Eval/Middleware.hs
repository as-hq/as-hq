module AS.Eval.Middleware where

import Prelude
import Data.List

import AS.Types
import AS.Eval as R
import AS.Util

-- here, we apply a stack of middlewares.

evalMiddleware :: ASCell -> IO ASCell
evalMiddleware cell = 
    evalInitExcel cell >>= 
        evalStream >>= 
            evalConnector >>=
                return

--------------------- middlewares ---------------------------------------------

evalInitExcel :: ASCell -> IO ASCell
evalInitExcel (Cell loc xp@(Expression rawXp Excel) val ts) = do
    newXp <- R.evalExcel xp
    return $ Cell loc newXp val ts
evalInitExcel cell = return cell

evalStream :: ASCell -> IO ASCell
evalStream cell@(Cell loc xp val ts) = if (containsStreamTag ts)
    then do
        time <- getASTime
        val' <- evalStreamHelper (streamSource . getStreamTag $ ts) time
        return $ Cell loc xp val' ts
    else return cell

evalStreamHelper :: StreamSource -> ASTime -> IO ASValue
evalStreamHelper src time = return NoValue -- TODO

evalConnector :: ASCell -> IO ASCell
evalConnector cell = return cell -- TODO