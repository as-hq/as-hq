{-# Language OverloadedStrings #-}

module AS.DB.Users where

import Database.Redis
import Control.Monad (void)
import System.Time
import System.Locale
import qualified Database.MySQL.Simple as DM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import AS.Prelude
import AS.Util
import AS.Config.Constants 
import AS.Serialize (encode, maybeDecode) 
import AS.Types.Network hiding (_userId, userId)
import qualified  AS.Types.Network as Network
import AS.Types.User
import AS.Types.Locations
import AS.Types.Window
import AS.Types.DB
import AS.Types.Messages

import qualified AS.DB.API as DB

--------------------------------------------------------------------------------
-- User session info in MySQL (for logging/replaying)

connInfo :: DM.ConnectInfo 
connInfo = DM.defaultConnectInfo {
 DM.connectPassword = "92c6b1451ebf141a2704be3772592b0b069940be" }
 
getFormattedTime :: IO String
getFormattedTime = do 
  ct <- getClockTime
  cal <- toCalendarTime ct
  return $ formatCalendarTime defaultTimeLocale "%a %b %e %H:%M:%S %Y" cal

-- Update user metadata (userId, sessionId) in the MySQL tables, 
-- called after each successful login
updateUserSession :: UserID -> SessionId -> IO ()
updateUserSession uId sId = void $ do
  let userId = T.unpack uId
  let sessionId = T.unpack sId
  time <- getFormattedTime
  conn <- DM.connect connInfo
  DM.execute conn "insert ignore into users (userId) values (?)" [userId]
  DM.executeMany conn 
    "insert into sessions (userId, sessionId, time) values (?,?,?)" 
    [(userId, sessionId, time)]

getAllSessions :: IO [SessionData]
getAllSessions = do 
  conn <- DM.connect connInfo 
  xs <- DM.query_ conn "select userId,sessionId,time from sessions"
  let f (uid, sid, time) = SessionData (T.pack uid) (T.pack sid) time
  return $ map f xs

--------------------------------------------------------------------------------
-- Creating users

createUserClient :: Connection -> WS.Connection -> User -> IO UserClient
createUserClient dbConn wsConn user = do
  let wid = user^.lastOpenWorkbook
  wb <- $fromJust <$> DB.getWorkbook dbConn wid 
  let sid = wb^.lastOpenSheet
  seshId <- T.pack <$> getUUID
  let window = Window 
                { _windowSheetId = sid
                , _windowWorkbookId = wb^.workbookId
                , _topLeft = makeCoord (-1) (-1)
                , _bottomRight = makeCoord (-1) (-1)
                }
  return $ UserClient (user^.userId) wsConn window seshId

createUser :: Connection -> UserID -> IO User
createUser conn uid = do
  wid <- view workbookId <$> DB.createWorkbook conn uid new_workbook_name
  let user  = User 
              { _userId = uid
              , _workbookIds = Set.singleton wid
              , _lastOpenWorkbook = wid
              }
  DB.setUser conn user 
  return user

produceUser :: Connection -> UserID -> IO User
produceUser conn uid = maybeM (createUser conn uid) id (DB.getUser conn uid)

--------------------------------------------------------------------------------
-- Reading users

-- | Get all sheets in the user's currently open workbook.
getOpenedSheets :: Connection -> UserID -> IO [Sheet]
getOpenedSheets conn uid = do
  mu <- DB.getUser conn uid
  case mu of 
    Just u -> do
      wb <- $fromJust <$> DB.getWorkbook conn (u^.lastOpenWorkbook)
      let sids = Set.toList $ wb^.workbookSheetIds
      map $fromJust <$> mapM (DB.getSheet conn) sids
    Nothing -> return []

getUserWorkbookRefs :: Connection -> UserID -> IO [WorkbookRef]
getUserWorkbookRefs conn uid = do
  user <- $fromJust <$> DB.getUser conn uid
  let wids = Set.toList $ user^.workbookIds
  wbs <- forM wids $ fmap $fromJust . DB.getWorkbook conn
  return $ map (\wb -> WorkbookRef (wb^.workbookId) (wb^.workbookName) (wb^.workbookOwner)) wbs

--------------------------------------------------------------------------------
-- Modifying/setting users

-- | Atomically (across state & DB) updates the current user,
-- returning the updated MessageContext for further consumption.
modifyUser :: MessageContext -> (User -> User) -> IO MessageContext
modifyUser msgctx f = do
  let conn = msgctx^.dbConnection
      state = msgctx^.messageState
      uc = msgctx^.userClient
      uid = view Network.userId uc
      seshId = uc^.userSessionId
  user <- $fromJust <$> DB.getUser conn uid
  let user' = f user
  DB.setUser conn user'
  if (uc^.userWindow.windowWorkbookId == user'^.lastOpenWorkbook) 
    then return msgctx
    else do
      let fuc = (& userWindow.windowWorkbookId .~ user'^.lastOpenWorkbook)
      modifyUserClientInState state seshId fuc
      return $ msgctx & userClient %~ fuc

-- | Atomically (across state & DB) updates the user's current open workbook
modifyCurrentWorkbook :: MessageContext -> (Workbook -> Workbook) -> IO Workbook
modifyCurrentWorkbook msgctx f = do
  let conn    = msgctx^.dbConnection
      uid     = messageUserId msgctx
      wid     = msgctx^.userClient.userWindow.windowWorkbookId
      state   = msgctx^.messageState
      seshId  = msgctx^.userClient.userSessionId
  wb <- f . $fromJust <$> DB.getWorkbook conn wid

  -- perform consistency checks
  wb' <- if Set.notMember (wb^.lastOpenSheet) (wb^.workbookSheetIds)
    then if Set.null (wb^.workbookSheetIds)
      -- if the user has no more sheets, make a new one.
      then do
        sid <- view sheetId <$> DB.createSheet conn uid new_sheet_name
        return $ wb & workbookSheetIds .~ Set.singleton sid & lastOpenSheet .~ sid
      else 
        -- else, pick one of the workbook's other sheets to open.
        let openedSheet = $head . Set.elems $ wb^.workbookSheetIds
        in return $ wb & lastOpenSheet .~ openedSheet
    else return wb

  -- apply writes.
  DB.setWorkbook conn wb'
  modifyUserClientInState state seshId 
    (& userWindow.windowSheetId .~ wb'^.lastOpenSheet)
  return wb'

--------------------------------------------------------------------------------