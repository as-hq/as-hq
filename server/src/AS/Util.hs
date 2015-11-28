module AS.Util where

import AS.Types.Cell
import AS.Types.Network
import AS.Logging

import Prelude
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