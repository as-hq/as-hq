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

-- | Given a message that's either a failure or updatea message, only send (to each user) the cells in their viewing window. 
-- Unless there was a failure, in which case send the failure message back to the original user. 
broadcastFiltered :: MVar ServerState -> ASUserClient -> ASServerMessage -> IO ()
broadcastFiltered _ orig msg@(ServerMessage _ (Failure _) _) = sendToOriginal orig msg
broadcastFiltered _ orig msg@(ServerMessage _ (DecoupleDuringEval) _) = sendToOriginal orig msg
broadcastFiltered state _ msg = broadcastFiltered' state msg

-- | Assumes message is not failure message. (Also assumes the payload is a sheet update.)
broadcastFiltered' :: MVar ServerState -> ASServerMessage -> IO ()
broadcastFiltered' state msg@(ServerMessage _ _ (PayloadSheetUpdate sheetUpdate)) = do 
  State ucs _ _ _ <- readMVar state
  mapM_ (sendFilteredSheetUpdate msg sheetUpdate) ucs

sendFilteredSheetUpdate :: ASServerMessage -> SheetUpdate -> ASUserClient -> IO ()
sendFilteredSheetUpdate msg (SheetUpdate (Update cs locs) rcs ds cfrs) uc = do
  let cells' = intersectViewingWindow cs (userWindow uc)
      locs'  = locs -- should run intersectViewingWindowLocs on this, but that's only implemented for [ASIndex] for now. 
      msg'   = msg { serverPayload = PayloadSheetUpdate $ SheetUpdate (Update cells' locs') rcs ds cfrs }
  sendMessage msg' (userConn uc)

sendToOriginal :: ASUserClient -> ASServerMessage -> IO ()
sendToOriginal uc msg = sendMessage msg (userConn uc)