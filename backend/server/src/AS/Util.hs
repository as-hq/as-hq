{-# LANGUAGE TemplateHaskell #-}
module AS.Util where

import Data.List (foldl', find)
import Data.Maybe (isJust)
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Data.Aeson
import Debug.Trace 
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL
import qualified Network.WebSockets as WS
import qualified Data.ByteString as B

import AS.Prelude
import AS.Logging
import AS.Types.Cell
import AS.Types.Network
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

sendMessage :: (ToJSON a, Show a) => a -> WS.Connection -> IO ()
sendMessage msg conn = do
  WS.sendTextData conn (encode msg)
  puts "SENT REPLY" 

-- | Generates a random number
getUniqueId :: IO String
getUniqueId = return . toString =<< nextRandom
