module AS.Reply (broadcastTo, broadcastErrOrUpdate, broadcastSheetUpdate, sendSheetUpdate, sendToOriginal) where

import Control.Concurrent
import Control.Concurrent.ParallelIO.Global (parallel_)
import Control.Lens

import AS.Types.Bar
import AS.Types.Cell
import AS.Types.Commits
import AS.Types.CondFormat
import AS.Types.Updates
import AS.Types.Messages
import AS.Types.Network
import AS.Types.RangeDescriptor
import AS.Types.User
import AS.Types.Errors
import AS.Types.Window
import AS.Util

----------------------------------------------------------------------------------------------------------------------------------------------
-- Sending message to user client(s)

broadcastTo :: ServerState -> [ASSheetId] -> ClientMessage -> IO ()
broadcastTo state sids message = do
  let ucs = state^.userClients
      ucsSheetIds = zip ucs (map userSheetId ucs)
      affectedUsers = map fst $ filter (\(_, sid) ->  sid `elem` sids) ucsSheetIds
  parallel_ $ map (\uc -> sendMessage message (uc^.userConn)) affectedUsers

-- Given a message that's either a failure or updatea message, only send (to each user) the cells in their viewing window. 
-- Unless there was a failure, in which case send the failure message back to the original user. 
broadcastErrOrUpdate :: MessageId -> ServerState -> ASUserClient -> Either ASExecError SheetUpdate -> IO ()
broadcastErrOrUpdate mid _ orig (Left err) = sendToOriginal orig $ makeErrorMessage mid err
broadcastErrOrUpdate mid state _ (Right update) = broadcastSheetUpdate mid state update

broadcastSheetUpdate :: MessageId -> ServerState -> SheetUpdate -> IO ()
broadcastSheetUpdate mid state sheetUpdate =
  parallel_ $ map
    (\uc -> sendSheetUpdate mid uc . filterSheetUpdate sheetUpdate . view userWindow $ uc) 
    (state^.userClients)

-- We are NOT filtering the cells we're deleting; we can't let frontend learn what cells got deleted lazily
-- since blank cells don't get saved in the database. Thus, if a cell gets blanked out, the user needs to know immediately. 
-- (We *do* only send back the deleted cells in the sheet though, as opposed to deleted cells everywhere.)
filterSheetUpdate :: SheetUpdate -> ASWindow -> SheetUpdate
filterSheetUpdate (SheetUpdate cu bu du cfru) win = update
  where 
    sid    = windowSheetId win
    -- #lens
    cu'    = filterUpdateByKey ((==) sid . refSheetId) cu 
    bu'    = filterUpdateByKey ((==) sid . barSheetId) bu
    du'    = filterUpdateByKey ((==) sid . view locSheetId . keyIndex) du
    -- We have to do this workaround because there's currently no easy way to get a sheetId from a 
    -- condFormatRuleId, so we just won't filter out the keys to delete, and we keep only the new rules
    -- that apply exclusively to cells in the current sheet. (...in any case, a conditional format rule
    -- should never be applicable over multiple sheets.) It's currently ok to ask frontend to delete
    -- conditional formatting rules that don't exist on the sheet (currently a noop), but we *really*
    -- shouldn't tell it to add rules from a separate sheet. (Alex 1/25)
    filteredCfrs = filter (all ((==) sid . rangeSheetId) . cellLocs) (cfru^.newVals)
    update = SheetUpdate cu' bu' du' (Update filteredCfrs (cfru^.oldKeys))

sendSheetUpdate :: MessageId -> ASUserClient -> SheetUpdate -> IO ()
sendSheetUpdate mid uc update = sendMessage (ClientMessage mid $ UpdateSheet update) (uc^.userConn)

sendToOriginal :: ASUserClient -> ClientMessage -> IO ()
sendToOriginal uc msg = sendMessage msg (uc^.userConn)
