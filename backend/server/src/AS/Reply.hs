module AS.Reply (broadcastTo, broadcastErrOrUpdate, broadcastSheetUpdate, sendSheetUpdate, sendToOriginal) where

import AS.Types.Cell
import AS.Types.Commits
import AS.Types.Updates
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User
import AS.Types.Errors
import AS.Util
import AS.Window

import Control.Concurrent
import Control.Lens

----------------------------------------------------------------------------------------------------------------------------------------------
-- Sending message to user client(s)

broadcastTo :: MVar ServerState -> [ASSheetId] -> ClientMessage -> IO ()
broadcastTo state sids message = do
  curState <- readMVar state
  let ucs = curState^.userClients
      ucsSheetIds = zip ucs (map userSheetId ucs)
      affectedUsers = map fst $ filter (\(_, sid) ->  sid `elem` sids) ucsSheetIds
  (flip mapM_) affectedUsers $ \(UserClient _ conn _ _) -> sendMessage message conn

-- Given a message that's either a failure or updatea message, only send (to each user) the cells in their viewing window. 
-- Unless there was a failure, in which case send the failure message back to the original user. 
broadcastErrOrUpdate :: MessageId -> MVar ServerState -> ASUserClient -> Either ASExecError SheetUpdate -> IO ()
broadcastErrOrUpdate mid _ orig (Left err) = sendToOriginal orig $ makeErrorMessage mid err
broadcastErrOrUpdate mid state _ (Right update) = broadcastSheetUpdate mid state update

broadcastSheetUpdate :: MessageId -> MVar ServerState -> SheetUpdate -> IO ()
broadcastSheetUpdate mid state sheetUpdate = do 
  curState <- readMVar state
  mapM_ 
    (\uc -> sendSheetUpdate mid uc . filterSheetUpdate sheetUpdate . userWindow $ uc) 
    (curState^.userClients)

-- We are NOT filtering the cells we're deleting; we can't let frontend learn what cells got deleted lazily
-- since blank cells don't get saved in the database. Thus, if a cell gets blanked out, the user needs to know immediately. 
-- (We *do* only send back the deleted cells in the sheet though, as opposed to deleted cells everywhere.)
filterSheetUpdate :: SheetUpdate -> ASWindow -> SheetUpdate
filterSheetUpdate (SheetUpdate (Update cs locs) rcs ds cfrs) win = update
  where 
    sid    = windowSheetId win
    -- cells' = intersectViewingWindow cs win -- only filtering by sheet now. no more viewing window bs
    cells' = filter ((==) sid . view (cellLocation.locSheetId)) cs
    locs'  = filter ((==) sid . refSheetId) locs 
    update = SheetUpdate (Update cells' locs') rcs ds cfrs

sendSheetUpdate :: MessageId -> ASUserClient -> SheetUpdate -> IO ()
sendSheetUpdate mid uc update = sendMessage (ClientMessage mid $ UpdateSheet update) (userConn uc)

sendToOriginal :: ASUserClient -> ClientMessage -> IO ()
sendToOriginal uc msg = sendMessage msg (userConn uc)