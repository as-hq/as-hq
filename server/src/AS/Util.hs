module AS.Util where

import AS.Types.Cell
import AS.Types.Network
import AS.Types.CellProps (emptyProps)
import AS.Logging

import Prelude

import qualified Data.Map as M
import Data.List (foldl')

import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import qualified Network.WebSockets as WS
import Data.Aeson
import Debug.Trace 

-------------------------------------------------------------------------------------------------------------------------
-- For debugging purposes only 

trace' :: (Show a) => String -> a -> a
trace' s x = trace ("\n\n\n" ++ s ++ "\n" ++ (show x) ++ "\n\n\n") x

testCell :: ASCell
testCell =  Cell (Index "" (1,1)) (Expression "hi" Python) (ValueS "Str") emptyProps

--------------------------------------------------------------------------------------------------------------
-- Misc

sendMessage :: (ToJSON a, Show a) => a -> WS.Connection -> IO ()
sendMessage msg conn = do
  WS.sendTextData conn (encode msg)
  printObj "Server sent message" msg

-- | Generates a random number
getUniqueId :: IO String
getUniqueId = return . toString =<< nextRandom

-- Inserts multiple elements into a map
insertMultiple :: (Ord k) => M.Map k v -> [k] -> [v] -> M.Map k v
insertMultiple mp keys values = foldl' (\curMap (key,value) -> M.insert key value curMap) mp assoc
  where assoc = zip keys values