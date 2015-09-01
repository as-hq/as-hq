module AS.Util where

import AS.Types
import Data.Time.Clock

--------------------------------------------------------------------------------------------------------------
-- | Conversions and Helpers

isJust :: Maybe ASCell -> Bool
isJust (Just c) = True
isJust Nothing = False

getCellMessage :: ASUser -> Either ASExecError [ASCell] -> ASMessage
getCellMessage user (Left e) = Message (userId user) Evaluate (Failure (generateErrorMessage e)) (PayloadN ())
getCellMessage user (Right cells) = Message (userId user) Evaluate Success (PayloadCL cells)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Error Handling

-- | Not yet implemented
generateErrorMessage :: ASExecError -> String
generateErrorMessage e = "hi"

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Time

getTime :: IO String
getTime = fmap (show . utctDayTime) getCurrentTime

printTimed :: String -> IO ()
printTimed str = do 
  time <- getTime
  putStrLn $ "[" ++ (show time) ++ "] " ++ str 

-- | Not yet implemented
getASTime :: IO ASTime
getASTime = return $ Time "hi" 1 2 3

----------------------------------------------------------------------------------------------------------------------------------------------
-- | viewing windows 

intersectViewingWindows :: [ASCell] -> [ASWindow] -> [ASCell]
intersectViewingWindows cells vws = concat $ map (intersectViewingWindow cells) vws 

intersectViewingWindow :: [ASCell] -> ASWindow -> [ASCell]
intersectViewingWindow cells vw = filter (inVW vw) cells
  where
    inVW :: ASWindow -> ASCell -> Bool
    inVW (Window wSheetId (tlc, tlr) (brc, brr)) (Cell (Index cSheetId (col,row)) _ _ _) = ((wSheetId==cSheetId) && (inRange col tlc (brc-tlc)) && (inRange row tlr (brr-tlr)))
    inRange :: Int -> Int -> Int -> Bool
    inRange elem start len = ((elem >= start) && (elem <= (start + len)))

fromRight :: Either a b -> b
fromRight (Right b) = b

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Users

updateMessageUser :: ASUserId -> ASMessage -> ASMessage
updateMessageUser uid (Message _ a r p) = Message uid a r p 

isGroupMember :: ASUserId -> ASUserGroup -> Bool
isGroupMember uid group = any ((==) uid) (groupMembers group)

isGroupAdmin :: ASUserId -> ASUserGroup -> Bool
isGroupAdmin uid group = any ((==) uid) (groupAdmins group)

isInEntity :: ASUserId -> ASEntity -> Bool
isInEntity uid (EntityGroup group) = isGroupMember uid group
isInEntity uid (EntityUser userid) = uid == userid 

hasPermissions :: ASUserId -> ASPermissions -> Bool
hasPermissions uid (Blacklist entities) = not $ any (isInEntity uid) entities
hasPermissions uid (Whitelist entities) = any (isInEntity uid) entities

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Tags

containsTrackingTag :: [ASCellTag] -> Bool
containsTrackingTag [] = False
containsTrackingTag ((Tracking):tags) = True
containsTrackingTag (tag:tags) = containsTrackingTag tags

hasVolatileTag :: ASCell -> Bool
hasVolatileTag = containsVolatileTag . cellTags

containsVolatileTag :: [ASCellTag] -> Bool
containsVolatileTag [] = False
containsVolatileTag ((Volatile):tags) = True
containsVolatileTag (tag:tags) = containsVolatileTag tags

getStreamTag :: [ASCellTag] -> Maybe Stream
getStreamTag [] = Nothing
getStreamTag ((StreamTag s):tags) = Just s
getStreamTag (tag:tags) = getStreamTag tags

-- | Would look at an expression like TODAY()+DAY() and get the stream tag (update every 10 seconds if any of today etc show up)
-- | TODO: implement
getStreamTagFromExpression :: ASExpression -> Maybe Stream
getStreamTagFromExpression xp = Nothing