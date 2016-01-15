{-# LANGUAGE TemplateHaskell #-}
module AS.Util where

import AS.Types.Cell
import AS.Types.Network
import AS.Types.CellProps (emptyProps)
import AS.Logging

import Prelude()
import AS.Prelude

import qualified Data.Map as M
import Data.List (foldl', find)
import Data.Maybe (isJust)

import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import qualified Network.WebSockets as WS
import Data.Aeson
import Debug.Trace 
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

-------------------------------------------------------------------------------------------------------------------------
-- For debugging purposes only 

trace' :: (Show a) => String -> a -> a
trace' s x = trace ("\n\n\n" ++ s ++ "\n" ++ (show x) ++ "\n\n\n") x

-- takes a cell transform to convert the test cell to something desired
testCell :: ASCell
testCell = Cell (Index "" (Coord 1 1)) (Expression "=1+1" Excel) NoValue emptyProps Nothing Nothing

--------------------------------------------------------------------------------------------------------------
-- Misc

sendMessage :: (ToJSON a, Show a) => a -> WS.Connection -> IO ()
sendMessage msg conn = do
  WS.sendTextData conn (encode msg)
  printObjForced "Server sent message" msg

-- | Generates a random number
getUniqueId :: IO String
getUniqueId = return . toString =<< nextRandom

-- Inserts multiple elements into a map
insertMultiple :: (Ord k) => M.Map k v -> [k] -> [v] -> M.Map k v
insertMultiple mp keys values = foldl' (\curMap (key,value) -> M.insert key value curMap) mp assoc
  where assoc = zip keys values
