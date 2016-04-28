module AS.Reply where 

import Control.Concurrent
import Control.Lens
import Control.Monad (unless)

import AS.Prelude

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
import AS.Logging

import Data.Aeson
import qualified Data.Text  as T
import qualified Data.Set   as S

import qualified Network.WebSockets as WS

----------------------------------------------------------------------------------------------------------------------------------------------
-- Sending actions

sendFailure :: MessageContext -> String -> IO ()
sendFailure msgctx = sendAction msgctx . ShowFailureMessage

sendAction :: MessageContext -> ClientAction -> IO ()
sendAction msgctx ac = sendMessage (msgctx^.userClient.userConn) (ClientMessage (msgctx^.messageId) ac) 

broadcastActionTo :: MessageContext -> [ASSheetId] -> ClientAction -> IO ()
broadcastActionTo msgctx sids ac = do
  st <- readContextualState msgctx
  let ucs = st^.userClients
      ucsSheetIds = zip ucs (map userSheetId ucs)
      affectedUsers = map fst $ filter (\(_, sid) ->  sid `elem` sids) ucsSheetIds
      message = ClientMessage (msgctx^.messageId) ac
  parForM_ affectedUsers $ \u -> 
    sendMessage (u^.userConn) message

----------------------------------------------------------------------------------------------------------------------------------------------
-- Sending message to user client(s)

-- Given a message that's either a failure or updatea message, only send (to each user) the cells in their viewing window. 
-- Unless there was a failure, in which case send the failure message back to the original user. 
broadcastErrOrUpdate :: MessageContext -> Either ASExecError SheetUpdate -> IO ()
broadcastErrOrUpdate msgctx (Left err) = 
  sendAction msgctx $ makeErrorAction err
broadcastErrOrUpdate msgctx (Right update) = broadcastSheetUpdate msgctx update

broadcastSheetUpdate :: MessageContext -> SheetUpdate -> IO ()
broadcastSheetUpdate msgctx sheetUpdate = do
  ucs <- view userClients <$> readContextualState msgctx
  parForM_ ucs $ \uc -> 
    sendSheetUpdate (msgctx^.messageId) uc . filterSheetUpdate sheetUpdate . view userWindow $ uc 

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
    filteredCfrs = S.filter (all ((==) sid . rangeSheetId) . cellLocs) (cfru^.newValsSet)
    update = SheetUpdate cu' bu' du' (Update filteredCfrs (cfru^.oldKeysSet))

sendSheetUpdate :: MessageId -> ASUserClient -> SheetUpdate -> IO ()
sendSheetUpdate mid uc update = sendMessage (uc^.userConn) (ClientMessage mid $ UpdateSheet update) 

----------------------------------------------------------------------------------------------------------------------------------------------
-- Low-level API

sendMessage :: WS.Connection -> ClientMessage -> IO ()
sendMessage conn msg = do
  WS.sendTextData conn (encode msg)
  puts $ "SENT REPLY TO " ++ T.unpack (clientMessageId msg)