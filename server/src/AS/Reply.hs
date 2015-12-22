module AS.Reply (broadcast, broadcastFiltered, broadcastFiltered', sendToOriginal) where

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

-- | Returns all the sheets referenced in a payload. Currently no support for PayloadWorkbookSheets
-- and PayloadWB, because those payloads suck. 
-- 
-- #needsrefactor kinda dumb. What if e.g. you want to update the header value on all clients who have
-- that sheet open? The payload doensn't always tell you which sheets you're on. 
sheetsInPayload :: ASPayload -> [ASSheetId]
sheetsInPayload (PayloadDelete rng cells) = (rangeSheetId rng):(map (locSheetId . cellLocation) cells)
sheetsInPayload (PayloadS (Sheet sid _ _)) = [sid]
-- TODO: timchu 12/14/15, sheetsInPayload does not have BarProps!
sheetsInPayload (PayloadCommit (Commit _ cdiff _ _)) = (map getSheet bf) ++ (map getSheet af)
  where
    bf = beforeVals cdiff
    af = afterVals cdiff
    getSheet = locSheetId . cellLocation
sheetsInPayload (PayloadN ()) = []

broadcast :: MVar ServerState -> ASServerMessage -> IO ()
broadcast state message = do
  (State ucs _ _ _) <- readMVar state
  let ucsSheetIds = zip ucs (map userSheetId ucs)
      affectedUsers = map fst $ filter (\(_, sid) ->  sid `elem` sheetsInPayload (serverPayload message)) ucsSheetIds
  (flip mapM_) affectedUsers $ \(UserClient _ conn _ _) -> sendMessage message conn

sendFilteredCells :: ASServerMessage -> [ASCell] -> ASUserClient -> IO ()
sendFilteredCells msg cells uc = do
  let cells' = intersectViewingWindow cells (userWindow uc)
      msg'   = msg { serverPayload = PayloadCL cells' }
  sendMessage msg' (userConn uc)

sendFilteredLocs :: ASServerMessage -> [ASIndex] -> ASUserClient -> IO ()
sendFilteredLocs msg locs uc = do
  let locs' = intersectViewingWindowLocs locs (userWindow uc)
      msg'  = msg { serverPayload = PayloadLL locs' }
  sendMessage msg' (userConn uc)

-- ::ALEX:: actually implement
sendFilteredSheetUpdate :: ASServerMessage -> SheetUpdate -> ASUserClient -> IO ()
sendFilteredSheetUpdate msg (SheetUpdate (Update cs locs) rcs ds cfrs) uc = do
  let cells' = intersectViewingWindow cs (userWindow uc)
      msg'  = msg { serverPayload = PayloadSheetUpdate $ SheetUpdate (Update cells' locs) rcs ds cfrs }
  sendMessage msg' (userConn uc)

-- | Given a message (commit, cells, etc), only send (to each user) the cells in their viewing window. 
-- Unless there was a failure, in which case send the failure message back to the original user. 
broadcastFiltered :: MVar ServerState -> ASUserClient -> ASServerMessage -> IO ()
broadcastFiltered _ orig msg@(ServerMessage _ (Failure _) _) = sendToOriginal orig msg
broadcastFiltered _ orig msg@(ServerMessage _ (DecoupleDuringEval) _) = sendToOriginal orig msg
broadcastFiltered state _ msg = broadcastFiltered' state msg

-- ::ALEX:: holy shit this is terrible
-- | Assumes message is not failure message. (Also assumes the payload is either cells or locations.)
broadcastFiltered' :: MVar ServerState -> ASServerMessage -> IO ()
broadcastFiltered' state msg@(ServerMessage _ _ (PayloadCL cells)) = do 
  State ucs _ _ _ <- readMVar state
  mapM_ (sendFilteredCells msg cells) ucs

broadcastFiltered' state msg@(ServerMessage _ _ (PayloadLL locs)) = do 
  State ucs _ _ _ <- readMVar state
  mapM_ (sendFilteredLocs msg locs) ucs

broadcastFiltered' state msg@(ServerMessage _ _ (PayloadSheetUpdate sheetUpdate)) = do 
  State ucs _ _ _ <- readMVar state
  mapM_ (sendFilteredSheetUpdate msg sheetUpdate) ucs

sendToOriginal :: ASUserClient -> ASServerMessage -> IO ()
sendToOriginal uc msg = sendMessage msg (userConn uc)
