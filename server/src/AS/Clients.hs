module AS.Clients where

import Prelude
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as L
import qualified Data.Text as T 
import qualified Network.WebSockets as WS
import Data.Aeson hiding (Success)

import Database.Redis hiding (decode, Message)

import AS.Types
import AS.Util as U
import AS.Config.Settings as S
import AS.DB.API as DB



-- from AS.Types
-- class Client c where
--   conn :: c -> WS.Connection
--   addClient :: c -> ServerState -> ServerState
--   removeClient :: c -> ServerState -> ServerState
--   initFromMessageAndConn :: ASMessage -> WS.Connection -> Maybe c

-------------------------------------------------------------------------------------------------------------------------
-- | Basic client comms

send :: ASMessage -> WS.Connection -> IO ()
send msg conn = WS.sendTextData conn (encode msg)

close :: WS.Connection -> IO ()
close conn = WS.sendClose conn ("Bye" :: T.Text)

-------------------------------------------------------------------------------------------------------------------------
-- | User Management

getUsers :: ServerState -> [ASUser]
getUsers (State us _ _) = us

userIdExists :: ASUserId -> ServerState -> Bool
userIdExists uid state = L.elem uid (L.map userId (getUsers state))

-- Assumes that user exists
getUserById :: ASUserId -> ServerState -> Maybe ASUser
getUserById uid (State allUsers _ _) = case (filter (\c -> (userId c == uid)) (allUsers)) of
  [] -> Nothing
  l -> Just $ L.head l

-- ::ALEX:: seems sketchy
modifyUser :: (ASUser -> ASUser) -> ASUser -> MVar ServerState -> IO ()
modifyUser func user state = modifyMVar_ state $ \(State users daemons conn) ->
  do 
    let users' = flip map users (\u -> if (u == user) then (func u) else u)
    return $ State users' daemons conn 

-------------------------------------------------------------------------------------------------------------------------
-- | Debugging

--getScrollCells :: Connection -> ASSheetId -> [ASLocation] -> IO [Maybe ASCell]
--getScrollCells conn sid locs = if ((sid == (T.pack "SHEET_ID")) && S.isDebug)
--  then do
--    let dlocs = concat $ map U.decomposeLocs locs
--    return $ map (\l -> Just $ Cell l (Expression "scrolled" Python) (ValueS (show . index $ l)) []) dlocs
--  else DB.getCells conn locs

getScrollCells :: Connection -> ASSheetId -> [ASLocation] -> IO [Maybe ASCell]
getScrollCells conn sid locs = DB.getCells conn locs