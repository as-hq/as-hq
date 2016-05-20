{-# LANGUAGE TemplateHaskell #-}
module AS.Util where

import Debug.Trace 

import Control.Concurrent (threadDelay)

import AS.Prelude
import AS.Logging
import AS.Types.Cell
import AS.Types.CellProps (emptyProps)

-------------------------------------------------------------------------------------------------------------------------
-- For debugging purposes only 

trace' :: (Show a) => String -> a -> a
trace' s x = trace ("\n\n\n" ++ s ++ "\n" ++ (show x) ++ "\n\n\n") x

-- | Takes a cell transform to convert the test cell to something desired
testCell :: ASCell
testCell = Cell (Index "" (makeCoord 1 1)) (Expression "=1+1" Excel) NoValue emptyProps Nothing Nothing

