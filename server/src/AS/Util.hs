module AS.Util where

import AS.Types

import Prelude
import Data.Time.Clock
import Data.Maybe (isNothing)
import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as U (toString)
import qualified Data.Text as T 
import qualified Data.List as L
import Control.Applicative hiding ((<|>), many)
import Data.Maybe (isNothing)

--------------------------------------------------------------------------------------------------------------
-- | Misc

lastN :: Int -> [a] -> [a]
lastN n xs = let m = length xs in drop (m-n) xs

-- always includes first element
every :: Int -> [a] -> [a]
every n = map head . takeWhile (not . null) . iterate (drop n)

(<++>) a b = (++) <$> a <*> b

(<:>) a b  = (:) <$> a <*> b

fromRight :: Either a b -> b
fromRight (Right b) = b

lookupLambda :: Eq a => (b -> a) -> a -> [b] -> Maybe b
lookupLambda func elm lst = case (filter (((==) elm) . func) lst) of
    [] -> Nothing
    (x:xs) -> Just x

max' :: Ord a => a -> a -> a
max' j k = if j > k
    then j
    else k

min' :: Ord a => a -> a -> a
min' j k = if j < k
    then j
    else k

fromJustList :: [Maybe a] -> [a]
fromJustList l = map (\(Just x) -> x) l

filterNothing :: [Maybe a] -> [a]
filterNothing l = fromJustList $ filter (not . isNothing) l

--------------------------------------------------------------------------------------------------------------
-- | Key-value manip functions

delFromAL :: Eq key => [(key, a)] -> key -> [(key, a)]
delFromAL l key = L.filter (\a -> (fst a) /= key) l

addToAL :: Eq key => [(key, elt)] -> key -> elt -> [(key, elt)]
addToAL l key value = (key, value) : delFromAL l key

--------------------------------------------------------------------------------------------------------------
-- | Conversions and Helpers

isJust :: Maybe ASCell -> Bool
isJust (Just c) = True
isJust Nothing = False

getCellMessage :: ASUser -> Either ASExecError [ASCell] -> ASMessage
getCellMessage user (Left e) = Message (userId user) Evaluate (Failure (generateErrorMessage e)) (PayloadN ())
getCellMessage user (Right cells) = Message (userId user) Evaluate Success (PayloadCL cells)

getBadLocs :: [ASLocation] -> [Maybe ASCell] -> [ASLocation]
getBadLocs locs mcells = map fst $ filter (\(l,c)->isNothing c) (zip locs mcells)

--getDBCellMessage :: ASUser -> [ASLocation] -> [Maybe ASCell] -> ASMessage
--getDBCellMessage user locs mcells = if any isNothing mcells
--  then getCellMessage user (Left (DBNothingException (getBadLocs locs mcells)))
--  else getCellMessage user (Right (map (\(Just c)->c) mcells))

-- bugfix for sending non-nothing locs (e.g. scrolling)
getDBCellMessage :: ASUser -> [ASLocation] -> [Maybe ASCell] -> ASMessage
getDBCellMessage user locs mcells = getCellMessage user (Right cells)
  where justCells = filter (not . isNothing) mcells 
        cells = map (\(Just x) -> x) justCells

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Error Handling

-- | Not yet implemented
generateErrorMessage :: ASExecError -> String
generateErrorMessage CopyNonexistentDependencies = "Dependencies nonexistent in copied cell expressions"
generateErrorMessage (DBNothingException _) = "Unable to fetch cells from database"

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
-- | Id management

getUniqueId :: IO T.Text
getUniqueId = return . T.pack . U.toString =<< nextRandom

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

updateWindow :: ASWindow -> ASUser -> ASUser
updateWindow window (User uid conn windows) = User uid conn windows'
    where windows' = flip map windows (\w -> if (windowSheetId w) == (windowSheetId window) then window else w)

getWindow :: ASSheetId -> ASUser -> Maybe ASWindow
getWindow sheetid user = lookupLambda windowSheetId sheetid (userWindows user)

getScrolledLocs :: ASWindow -> ASWindow -> [ASLocation]
getScrolledLocs (Window _ (-1,-1) (-1,-1)) (Window sheetid tl br) = [(Range sheetid (tl, br))] 
getScrolledLocs (Window _ (y,x) (y2,x2)) (Window sheetid tl@(y',x') br@(y2',x2')) = getUncoveredLocs sheetid overlapping (tl, br)
    where overlapping = ((max' y y', max' x x'), (min' y2 y2', min' x2 x2'))

getUncoveredLocs :: ASSheetId -> ((Int,Int), (Int,Int)) -> ((Int,Int), (Int,Int)) -> [ASLocation]
getUncoveredLocs sheet (tlo, bro) (tlw, brw) = [Range sheet corners | corners <- cs]
    where 
      trw = (fst brw, snd tlw)
      blw = (fst tlw, snd brw)
      tro = (fst bro, snd tlo)
      blo = (fst tlo, snd bro)
      cs = [(tlw, tro), (trw, bro), (brw, blo), (blw, tlo)]

getAllUserWindows :: ServerState -> [(ASUserId, [ASWindow])]
getAllUserWindows state = map (\(u,d) -> (userId u, userWindows u)) (userList state)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Locations

decomposeLocs :: ASLocation -> [ASLocation]
decomposeLocs loc = case loc of 
  (Index sheet a) -> [loc]
  (Range sheet (ul, lr)) -> [Index sheet (x,y) | x <- [startx..endx], y <- [starty..endy] ]
    where 
      startx = min' (fst ul) (fst lr)
      endx = max' (fst ul) (fst lr)
      starty = min' (snd ul) (snd lr)
      endy = max' (snd ul) (snd lr)

matchSheets :: [ASWorkbook] -> [ASSheet] -> [WorkbookSheet]
matchSheets ws ss = [WorkbookSheet (workbookName w) (fromJustList $ lookupSheets w ss) | w <- ws]
  where lookupSheets workbook sheets = map (\sid -> lookupLambda sheetId sid sheets) (workbookSheets workbook)

shiftLoc :: (Int, Int) -> ASLocation -> ASLocation
shiftLoc (dy, dx) (Index sh (y,x)) = Index sh (y+dy, x+dx)
shiftLoc (dy, dx) (Range sh ((y,x),(y2,x2))) = Range sh ((y+dy, x+dx), (y2+dy, x2+dx))

getTopLeft :: ASLocation -> ASLocation
getTopLeft (Range sh (tl,_)) = Index sh tl
getTopLeft loc = loc

getOffsetBetweenLocs :: ASLocation -> ASLocation -> (Int, Int)
getOffsetBetweenLocs from to = getOffsetFromIndices from' to'
  where 
    from' = getTopLeft from
    to' = getTopLeft to
    getOffsetFromIndices (Index _ (y, x)) (Index _ (y', x')) = (y'-y, x'-x)

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