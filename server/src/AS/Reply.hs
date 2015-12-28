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
broadcastErrOrUpdate :: MVar ServerState -> ASUserClient -> Either ASExecError SheetUpdate -> IO ()
broadcastErrOrUpdate _ orig (Left err) = sendToOriginal orig $ makeErrorMessage err
broadcastErrOrUpdate state _ (Right update) = broadcastSheetUpdate state update

broadcastSheetUpdate :: MVar ServerState -> SheetUpdate -> IO ()
broadcastSheetUpdate state sheetUpdate = do 
  State ucs _ _ _ <- readMVar state
  mapM_ (\uc -> sendSheetUpdate uc . filterSheetUpdate sheetUpdate . userWindow $ uc) ucs

-- We are NOT filtering the cells we're deleting; we can't let frontend learn what cells got deleted lazily
-- since blank cells don't get saved in the database. Thus, if a cell gets blanked out, the user needs to know immediately. 
-- (We *do* only send back the deleted cells in the sheet though, as opposed to deleted cells everywhere.)
filterSheetUpdate :: SheetUpdate -> ASWindow -> SheetUpdate
filterSheetUpdate (SheetUpdate (Update cs locs) rcs ds cfrs) win = update
  where 
    sid    = windowSheetId win
    cells' = intersectViewingWindow cs win
    locs'  = filter ((==) sid . refSheetId) locs 
    update = SheetUpdate (Update cells' locs') rcs ds cfrs

sendSheetUpdate :: ASUserClient -> SheetUpdate -> IO ()
sendSheetUpdate uc update = sendMessage (ServerMessage $ UpdateSheet update) (userConn uc)

sendToOriginal :: ASUserClient -> ASServerMessage -> IO ()
sendToOriginal uc msg = sendMessage msg (userConn uc)