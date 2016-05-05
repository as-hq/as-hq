{-# LANGUAGE TemplateHaskell #-}
module AS.Util where

import Data.UUID.V1 (nextUUID)
import Data.UUID    (toString)
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

-------------------------------------------------------------------------------------------------------------------------
-- Misc

-- | a hyphenated alphanumeric UUID 
getUUID :: IO String
getUUID = do
  u <- nextUUID
  case u of 
    Just u  -> return $ toString u
    -- according to https://hackage.haskell.org/package/uuid-1.3.12/docs/Data-UUID-V1.html,
    -- `nextUUID` returns Nothing if you request UUIDs too quickly.
    Nothing -> putStrLn "could not generate UUID!" >> threadDelay 100 >> getUUID
