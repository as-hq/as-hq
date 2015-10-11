module AS.Util where

import AS.Types.Core

import Prelude
import Data.Time.Clock
import Data.Maybe (isNothing)
import Data.UUID.V4 (nextRandom)
import Data.Aeson hiding (Success)
import qualified Network.WebSockets as WS
import qualified Data.UUID as U (toString)
import qualified Data.Text as T 
import qualified Data.List as L
import qualified Data.Set as S
import Control.Applicative hiding ((<|>), many)
import Data.Maybe (isNothing,catMaybes)

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

-------------------------------------------------------------------------------------------------------------------------
-- Initializations

initDaemonFromMessageAndConn :: ASClientMessage -> WS.Connection -> ASDaemonClient
initDaemonFromMessageAndConn (ClientMessage _ (PayloadDaemonInit (ASInitDaemonConnection uid loc))) c = DaemonClient loc c uid

initUserFromMessageAndConn :: ASClientMessage -> WS.Connection -> IO ASUserClient
initUserFromMessageAndConn (ClientMessage _ (PayloadInit (ASInitConnection uid))) c = do 
    time <- getTime
    return $ UserClient uid c [initialViewingWindow] $ T.pack ((show uid) ++ (show time))

--------------------------------------------------------------------------------------------------------------
-- Misc 

sendMessage :: (ToJSON a, Show a) => a -> WS.Connection -> IO ()
sendMessage msg conn = do 
  WS.sendTextData conn (encode msg)
  printWithTime $ "Server sent message: " ++ (show msg)

lastN :: Int -> [a] -> [a]
lastN n xs = let m = length xs in drop (m-n) xs

-- faster
lastN' :: Int -> [a] -> [a]
lastN' n xs = L.foldl' (const . drop 1) xs (drop n xs)

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

tuple3 :: a -> b -> c -> (a,b,c)
tuple3 a b c = (a,b,c)

filterNothing :: [Maybe a] -> [a]
filterNothing l = catMaybes $ filter (not . isNothing) l

isoFilter :: (a -> Bool) -> [a] -> [b] -> [b]
isoFilter f pimg img = map snd $ filter (\(a,b) -> f a) $ zip pimg img

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf [] _ = True
isSubsetOf f s = all id $ map (\i-> L.elem i s) f

maxBy :: Ord a => (b -> a) -> [b] -> b
maxBy f (x:xs) = helper x (f x) $ zip (map f xs) xs
  where
    helper m _ [] = m
    helper m fm ((fx, x):fxs) = if (fx > fm)
      then helper x fx fxs
      else helper m fm fxs

minBy :: Ord a => (b -> a) -> [b] -> b
minBy f (x:xs) = helper x (f x) $ zip (map f xs) xs
  where
    helper m _ [] = m
    helper m fm ((fx, x):fxs) = if (fx < fm)
      then helper x fx fxs
      else helper m fm fxs

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isAllRight :: [Either a b] -> Bool
isAllRight results = all id $ map isRight results 

deleteSubset :: (Eq a) => [a] -> [a] -> [a]
deleteSubset subset = filter (\e -> L.notElem e subset)

isNonEmptyCell :: ASCell -> Bool 
isNonEmptyCell = ((/=) "") . expression . cellExpression

--------------------------------------------------------------------------------------------------------------
-- Key-value manip functions

delFromAL :: Eq key => [(key, a)] -> key -> [(key, a)]
delFromAL l key = L.filter (\a -> (fst a) /= key) l

addToAL :: Eq key => [(key, elt)] -> key -> elt -> [(key, elt)]
addToAL l key value = (key, value) : delFromAL l key

--------------------------------------------------------------------------------------------------------------
-- Conversions and Helpers

isJust :: Maybe ASCell -> Bool
isJust (Just c) = True
isJust Nothing = False

getCellMessage :: Either ASExecError [ASCell] -> ASServerMessage
getCellMessage (Left e) = ServerMessage Evaluate (Failure (generateErrorMessage e)) (PayloadN ())
getCellMessage (Right cells) = ServerMessage Evaluate Success (PayloadCL cells)

-- getBadLocs :: [ASReference] -> [Maybe ASCell] -> [ASReference]
-- getBadLocs locs mcells = map fst $ filter (\(l,c)->isNothing c) (zip locs mcells)

getDBCellMessage :: [Maybe ASCell] -> ASServerMessage
getDBCellMessage mcells = getCellMessage (Right cells)
  where justCells = filter (not . isNothing) mcells 
        cells = map (\(Just x) -> x) justCells

----------------------------------------------------------------------------------------------------------------------------------------------
-- Error Handling

-- | Not yet implemented
generateErrorMessage :: ASExecError -> String
generateErrorMessage e = case e of 
  CopyNonexistentDependencies -> "Some dependencies nonexistent in copied cell expressions."
  (DBNothingException _)      -> "Unable to fetch cells from database."
  ExpressionNotEvaluable      -> "Expression not does not contain evaluable statement."
  ExecError                   -> "Error while evaluating expression."
  (ExcelSyntaxError s)        -> "Formula syntax error: " ++ s
  SyntaxError                 -> "Syntax error."


----------------------------------------------------------------------------------------------------------------------------------------------
-- Time
getTime :: IO String
getTime = fmap (show . utctDayTime) getCurrentTime

printWithTime :: String -> IO ()
printWithTime str = do 
  time <- getTime
  putStrLn $ "[" ++ (show time) ++ "] " ++ str 

printWithTimeT :: String -> EitherTExec ()
printWithTimeT = lift . printWithTime

-- | For debugging purposes
printDebug :: (Show a) => String -> a -> IO ()
printDebug name obj = printWithTime ("\n\n" ++ name ++ ": " ++ (show obj))

-- | Not yet implemented
getASTime :: IO ASTime
getASTime = return $ Time "hi" 1 2 3

----------------------------------------------------------------------------------------------------------------------------------------------
-- Id management

getUniqueId :: IO T.Text
getUniqueId = return . T.pack . U.toString =<< nextRandom

----------------------------------------------------------------------------------------------------------------------------------------------
-- viewing windows 

intersectViewingWindows :: [ASCell] -> [ASWindow] -> [ASCell]
intersectViewingWindows cells vws = concat $ map (intersectViewingWindow cells) vws 
  where
    intersectViewingWindow :: [ASCell] -> ASWindow -> [ASCell]
    intersectViewingWindow cells vw = filter (inViewingWindow vw) cells
    inViewingWindow :: ASWindow -> ASCell -> Bool
    inViewingWindow (Window wSheetId (tlc, tlr) (brc, brr)) (Cell (Index cSheetId (col,row)) _ _ _) = ((wSheetId==cSheetId) && (inRange col tlc (brc-tlc)) && (inRange row tlr (brr-tlr)))
    inRange :: Int -> Int -> Int -> Bool
    inRange elem start len = ((elem >= start) && (elem <= (start + len)))

-- new function, so that we don't have to do the extra filter/lookup by using just one
intersectViewingWindowsLocs :: [ASIndex] -> [ASWindow] -> [ASIndex]
intersectViewingWindowsLocs locs vws = concat $ map (intersectViewingWindow locs) vws 
  where
    intersectViewingWindow :: [ASIndex] -> ASWindow -> [ASIndex]
    intersectViewingWindow locs vw = filter (inViewingWindow vw) locs
    inViewingWindow :: ASWindow -> ASIndex -> Bool
    inViewingWindow (Window wSheetId (tlc, tlr) (brc, brr)) (Index cSheetId (col,row)) = 
      ((wSheetId==cSheetId) && (inRange col tlc (brc-tlc)) && (inRange row tlr (brr-tlr)))
    inRange :: Int -> Int -> Int -> Bool
    inRange elem start len = ((elem >= start) && (elem <= (start + len)))

updateWindow :: ASWindow -> ASUserClient -> ASUserClient
updateWindow window (UserClient uid conn windows sid) = UserClient uid conn windows' sid
    where windows' = flip map windows (\w -> if (windowSheetId w) == (windowSheetId window) then window else w)

getWindow :: ASSheetId -> ASUserClient -> Maybe ASWindow
getWindow sheetid user = lookupLambda windowSheetId sheetid (windows user)

getScrolledLocs :: ASWindow -> ASWindow -> [ASRange]
getScrolledLocs (Window _ (-1,-1) (-1,-1)) (Window sheetid tl br) = [(Range sheetid (tl, br))] 
getScrolledLocs (Window _ (y,x) (y2,x2)) (Window sheetid tl@(y',x') br@(y2',x2')) = getUncoveredLocs sheetid overlapping (tl, br)
    where overlapping = ((max y y', max x x'), (min y2 y2', min x2 x2'))

getUncoveredLocs :: ASSheetId -> ((Int,Int), (Int,Int)) -> ((Int,Int), (Int,Int)) -> [ASRange]
getUncoveredLocs sheet (tlo, bro) (tlw, brw) = [Range sheet corners | corners <- cs]
    where 
      trw = (fst brw, snd tlw)
      blw = (fst tlw, snd brw)
      tro = (fst bro, snd tlo)
      blo = (fst tlo, snd bro)
      cs = [(tlw, tro), (trw, bro), (brw, blo), (blw, tlo)]

getAllUserWindows :: ServerState -> [(ASUserId, [ASWindow])]
getAllUserWindows state = map (\u -> (userId u, windows u)) (userClients state)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Locations

isListMember :: ASCell -> Bool
isListMember (Cell _ _ _ ts) = any id $ map (\t -> case t of 
  (ListMember _) -> True
  _ -> False) ts

getListTag :: ASCell -> Maybe ASCellTag
getListTag (Cell _ _ _ ts) = getTag foundTags
  where 
    foundTags = filter (\t -> case t of 
      (ListMember _) -> True
      _ -> False) ts
    getTag [] = Nothing
    getTag (t:[]) = Just t

-- | ASReference is either a cell index, range, or column. When decomposeLocs takes a range, it returns
-- the list of indices that compose the range. When it takes in an index, it returns a list consisting
-- of just that index. It cannot take in a column. 
refToIndices :: ASReference -> [ASIndex]
refToIndices loc = case loc of 
  (IndexRef ind) -> [ind]
  (RangeRef r) -> rangeToIndices r
-- decomposeLocs :: ASReference -> [ASIndex]
-- decomposeLocs loc = case loc of 
--   (IndexRef ind) -> [ind]
  -- (RangeRef (Range sheet (ul, lr))) -> [Index sheet (x,y) | x <- [startx..endx], y <- [starty..endy] ]
  --   where 
  --     startx = min (fst ul) (fst lr)
  --     endx = max (fst ul) (fst lr)
  --     starty = min (snd ul) (snd lr)
  --     endy = max (snd ul) (snd lr)

rangeToIndices :: ASRange -> [ASIndex]
rangeToIndices (Range sheet (ul, lr)) = [Index sheet (x,y) | x <- [startx..endx], y <- [starty..endy] ]
  where 
    startx = min (fst ul) (fst lr)
    endx = max (fst ul) (fst lr)
    starty = min (snd ul) (snd lr)
    endy = max (snd ul) (snd lr)

matchSheets :: [ASWorkbook] -> [ASSheet] -> [WorkbookSheet]
matchSheets ws ss = [WorkbookSheet (workbookName w) (catMaybes $ lookupSheets w ss) | w <- ws]
  where lookupSheets workbook sheets = map (\sid -> lookupLambda sheetId sid sheets) (workbookSheets workbook)


shiftLoc :: (Int, Int) -> ASReference -> ASReference
shiftLoc (dy, dx) (IndexRef (Index sh (y,x))) = IndexRef $ Index sh (y+dy, x+dx)
shiftLoc (dy, dx) (RangeRef (Range sh ((y,x),(y2,x2)))) = RangeRef $ Range sh ((y+dy, x+dx), (y2+dy, x2+dx))

shiftInd :: (Int, Int) -> ASIndex -> ASIndex
shiftInd (dy, dx) (Index sh (y,x)) = Index sh (y+dy, x+dx)

getTopLeft :: ASRange -> ASIndex
getTopLeft (Range sh (tl,_)) = Index sh tl

getRangeDims :: ASRange -> (Int, Int)
getRangeDims (Range _ ((y1, x1), (y2, x2))) = (1 + abs (y2 - y1), 1 + abs (x2 - x1))

getPasteOffsets :: ASRange -> ASRange -> [(Int, Int)]
getPasteOffsets from to = offsets 
  where 
    (fromYDim, fromXDim) = getRangeDims from 
    (toYDim, toXDim) = getRangeDims to 
    yRep = max 1 (toYDim `div` fromYDim)
    xRep = max 1 (toXDim `div` fromXDim)
    (topYOffset, topXOffset) = getIndicesOffset (getTopLeft from) (getTopLeft to)
    yRepOffsets = take yRep [0,fromYDim..]
    xRepOffsets = take xRep [0,fromXDim..]
    offsets = [(topYOffset + y, topXOffset + x) | y <- yRepOffsets, x <- xRepOffsets]


getIndicesOffset :: ASIndex -> ASIndex -> (Int, Int)
getIndicesOffset (Index _ (y, x)) (Index _ (y', x')) = (y'-y, x'-x)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Users

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
-- Tags

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

----------------------------------------------------------------------------------------------------------------------
-- | Testing

testLocs :: Int -> [ASIndex]
testLocs n = [Index "" (i,1) | i <-[1..n]]

testCells :: Int -> [ASCell]
testCells n =  L.map (\l -> Cell (Index "" (l,1)) (Expression "hi" Python) (ValueS "Str") []) [1..n]
