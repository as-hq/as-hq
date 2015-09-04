module AS.Clients where

import Prelude
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as L
import qualified Data.Text as T 
import qualified Network.WebSockets as WS
import Data.Aeson hiding (Success)

import AS.Types
import AS.Util as U
import AS.Config.Settings as S
import AS.DB.API as DB

-------------------------------------------------------------------------------------------------------------------------
-- | Basic client comms

send :: ASMessage -> WS.Connection -> IO ()
send msg conn = WS.sendTextData conn (encode msg)

close :: WS.Connection -> IO ()
close conn = WS.sendClose conn ("Bye" :: T.Text)

-------------------------------------------------------------------------------------------------------------------------
-- | User Management

getUsers :: ServerState -> [ASUser]
getUsers (State s) = L.map fst s

userIdExists :: ASUserId -> ServerState -> Bool
userIdExists uid state = L.elem uid (L.map userId (getUsers state))

-- Assumes that user exists
getUserById :: ASUserId -> ServerState -> Maybe ASUser
getUserById uid (State allUsers) = case (filter (\c -> (userId c == uid)) (L.map fst allUsers)) of
  [] -> Nothing
  l -> Just $ L.head l

addUser :: ASUser -> ServerState -> ServerState
addUser user state@(State users) = state'
  where
    l = L.lookup user users
    state' = case l of 
      Nothing -> State ((user,[]):users)
      Just _ -> state

removeUser :: ASUser -> ServerState -> IO ServerState -- need to deal with daemons 
removeUser user s@(State us) = do 
  let daemons = L.lookup user us
  case daemons of 
    Nothing -> return s
    Just d -> do 
      mapM_ close (L.map daemonConn d)
      let us' = delFromAL us user
      return $ State us'

modifyUser :: (ASUser -> ASUser) -> ASUser -> MVar ServerState -> IO ()
modifyUser func user state = modifyMVar_ state $ \(State users) -> 
    return $ State $ flip map users (\u -> 
        if (fst u) == user
            then (func (fst u), snd u)
            else u)

-------------------------------------------------------------------------------------------------------------------------
-- | Debugging

getScrollCells :: ASSheetId -> [ASLocation] -> IO [Maybe ASCell]
getScrollCells sid locs = if ((sid == (T.pack "TEST_SHEET_ID")) && S.isDebug)
  then do
    let dlocs = concat $ map U.decomposeLocs locs
    return $ map (\l -> Just $ Cell l (Expression "scrolled" Python) (ValueS (show . index $ l)) []) dlocs
  else DB.getCells locs