module AS.Reply (broadcastTo, broadcastFiltered, broadcastFiltered', sendToOriginal) where

import AS.Types.Cell
import AS.Types.Commits
import AS.Types.Updates
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User
import AS.Util
import AS.Window

import Control.Concurrent

----------------------------------------------------------------------------------------------------------------------------------------------
-- Sending message to user client(s)

broadcastTo :: MVar ServerState -> [ASSheetId] -> ASServerMessage -> IO ()
broadcastTo state sids message = do
  (State ucs _ _ _) <- readMVar state
  let ucsSheetIds = zip ucs (map userSheetId ucs)
      affectedUsers = map fst $ filter (\(_, sid) ->  sid `elem` sids) ucsSheetIds
  (flip mapM_) affectedUsers $ \(UserClient _ conn _ _) -> sendMessage message conn

-- ::ALEX::
-- | Given a message that's either a failure or updatea message, only send (to each user) the cells in their viewing window. 
-- Unless there was a failure, in which case send the failure message back to the original user. 
broadcastFiltered :: MVar ServerState -> ASUserClient -> ASServerMessage -> IO ()
broadcastFiltered _ orig msg@(ServerMessage (ShowFailureMessage _)) = sendToOriginal orig msg
broadcastFiltered _ orig msg@(ServerMessage AskDecouple) = sendToOriginal orig msg
broadcastFiltered state _ msg = broadcastFiltered' state msg

-- | Assumes message is not failure message. (Also assumes the payload is a sheet update.)
broadcastFiltered' :: MVar ServerState -> ASServerMessage -> IO ()
broadcastFiltered' state msg@(ServerMessage (UpdateSheet sheetUpdate)) = do 
  State ucs _ _ _ <- readMVar state
  mapM_ (sendFilteredSheetUpdate msg sheetUpdate) ucs

-- We are NOT filtering the cells we're deleting; we can't let frontend learn what cells got deleted lazily
-- since blank cells don't get saved in the database. Thus, if a cell gets blanked out, the user needs to know immediately. 
-- (We do filter by sheet though.)
sendFilteredSheetUpdate :: ASServerMessage -> SheetUpdate -> ASUserClient -> IO ()
sendFilteredSheetUpdate msg (SheetUpdate (Update cs locs) rcs ds cfrs) uc = do
  let sid    = userSheetId uc
      cells' = intersectViewingWindow cs (userWindow uc)
      locs'  = filter ((==) sid . refSheetId) locs 
      msg'   = msg { serverAction = UpdateSheet $ SheetUpdate (Update cells' locs') rcs ds cfrs }
  sendMessage msg' (userConn uc)

sendToOriginal :: ASUserClient -> ASServerMessage -> IO ()
sendToOriginal uc msg = sendMessage msg (userConn uc)