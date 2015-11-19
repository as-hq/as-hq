module AS.Util where

import AS.Types.Core

import Prelude
import AS.Config.Paths
import Data.Time.Clock
import Data.Maybe (isNothing,fromJust)
import Data.UUID.V4 (nextRandom)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P

import Data.Aeson hiding (Success)
import qualified Network.WebSockets as WS
import qualified Data.UUID as U (toString)
import qualified Data.Text as T
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Set as S

import Control.Applicative hiding ((<|>), many)
import Data.Maybe (isNothing,catMaybes,fromJust)
import Data.List.Split (chunksOf)

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

import Control.Exception (catch, finally, SomeException)
import Control.Monad.IO.Class(liftIO)
import System.IO.Unsafe (unsafePerformIO)

import Data.Ord

import Debug.Trace 

-------------------------------------------------------------------------------------------------------------------------
-- For debugging purposes only 

trace' :: (Show a) => String -> a -> a
trace' s x = trace ("\n\n\n" ++ s ++ "\n" ++ (show x) ++ "\n\n\n") x

unsafePrint :: String -> ()
unsafePrint str = unsafePerformIO $ putStrLn str

-------------------------------------------------------------------------------------------------------------------------
-- Initializations

initDaemonFromMessageAndConn :: ASClientMessage -> WS.Connection -> ASDaemonClient
initDaemonFromMessageAndConn (ClientMessage _ (PayloadDaemonInit (ASInitDaemonConnection uid loc))) c = DaemonClient loc c uid

initUserFromMessageAndConn :: ASClientMessage -> WS.Connection -> IO ASUserClient
initUserFromMessageAndConn (ClientMessage _ (PayloadInit (ASInitConnection uid sid))) c = do
    time <- getTime
    return $ UserClient uid c (Window sid (-1,-1) (-1,-1)) $ T.pack ((show uid) ++ (show time))

--------------------------------------------------------------------------------------------------------------
-- Misc

truncated :: String -> String
truncated str
  | length str < 500 = str 
  | otherwise = (take 500 str) ++ ("... [Truncated]")

-- Alex 10/22: seems kind of ugly. 
differentTagType :: ASCellTag -> ASCellTag -> Bool
differentTagType (Color _) (Color _) = False
differentTagType (Size _) (Size _) = False
differentTagType (Format _) (Format _) = False
differentTagType (StreamTag _) (StreamTag _) = False
differentTagType Tracking Tracking = False
differentTagType Volatile Volatile = False
differentTagType (ReadOnly _) (ReadOnly _) = False
differentTagType (ImageData _ _ _ _) (ImageData _ _ _ _) = False
differentTagType _ _ = True

getCellFormatType :: ASCell -> Maybe FormatType
getCellFormatType cell = ft
  where
    ts = cellTags cell
    ft = case L.find (\t -> not $ differentTagType (Format NoFormat) t) ts of
      Nothing -> Nothing
      Just (Format ft') -> Just ft'

sendMessage :: (ToJSON a, Show a) => a -> WS.Connection -> IO ()
sendMessage msg conn = do
  WS.sendTextData conn (encode msg)
  printObj "Server sent message" msg

lastN :: Int -> [a] -> [a]
lastN n xs = let m = length xs in drop (m-n) xs

-- faster
lastN' :: Int -> [a] -> [a]
lastN' n xs = L.foldl' (const . drop 1) xs (drop n xs)

-- always includes first element
every :: Int -> [a] -> [a]
every n = map head . takeWhile (not . null) . iterate (drop n)

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

zipFilter :: [(a, Bool)] -> [a]
zipFilter zipped = map fst $ filter (\(_,b) -> b) zipped

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

isEmptyCell :: ASCell -> Bool
isEmptyCell c = (null $ cellTags c) && (null . xpString $ cellExpression c)

liftEitherTuple :: Either b (a0, a1) -> (Either b a0, Either b a1)
liftEitherTuple (Left b) = (Left b, Left b)
liftEitherTuple (Right (a0, a1)) = (Right a0, Right a1)

liftListTuple :: [([a],[b])] -> ([a], [b])
liftListTuple t = (concat $ map fst t, concat $ map snd t)

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy delimiter = foldr f [[]]
  where f c l@(x:xs) | c == delimiter = []:l
                     | otherwise = (c:x):xs

catchEitherT :: EitherTExec (Formatted CompositeValue) -> EitherTExec (Formatted CompositeValue)
catchEitherT a = do
  result <- liftIO $ catch (runEitherT a) whenCaught
  case result of
    Left e -> left e
    Right e -> right e
    where 
      whenCaught :: SomeException -> IO (Either ASExecError (Formatted CompositeValue))
      whenCaught e = return . Right $ Formatted (CellValue $ ValueError (show e) "StdErr") Nothing

fromDouble :: Double -> Either Double Int
fromDouble x = if (x == xInt)
  then Right $ fromInteger (round x)
  else Left x
  where xInt = fromInteger (round x)

reshapeList :: [a] -> Dimensions -> [[a]]
reshapeList xs (height, width) = case width of 
  1 -> error "cannot reshape into 1-dimensional list"
  _ -> chunksOf width xs

--------------------------------------------------------------------------------------------------------------
-- Key-value manip functions

delFromAL :: Eq key => [(key, a)] -> key -> [(key, a)]
delFromAL l key = L.filter (\a -> (fst a) /= key) l

addToAL :: Eq key => [(key, elt)] -> key -> elt -> [(key, elt)]
addToAL l key value = (key, value) : delFromAL l key

--------------------------------------------------------------------------------------------------------------
-- Conversions and Helpers

-- | When you have a list of cells from an eval request, this function constructs
-- the message to send back. 
makeUpdateMessage :: Either ASExecError [ASCell] -> ASServerMessage
makeUpdateMessage (Left e) = ServerMessage Update (Failure (generateErrorMessage e)) (PayloadN ())
makeUpdateMessage (Right cells) = ServerMessage Update Success (PayloadCL cells)

-- getBadLocs :: [ASReference] -> [Maybe ASCell] -> [ASReference]
-- getBadLocs locs mcells = map fst $ filter (\(l,c)->isNothing c) (zip locs mcells)

-- | Poorly named. When you have a list of cells from a get request, this function constructs
-- the message to send back. 
makeGetMessage :: [ASCell] -> ASServerMessage
makeGetMessage cells = changeMessageAction Get $ makeUpdateMessage (Right cells)

makeUpdateWindowMessage :: [ASCell] -> ASServerMessage
makeUpdateWindowMessage cells = changeMessageAction UpdateWindow $ makeUpdateMessage (Right cells)

-- | Makes a delete message from an Update message and a list of locs to delete
makeDeleteMessage :: ASRange -> ASServerMessage -> ASServerMessage
makeDeleteMessage _ s@(ServerMessage _ (Failure _) _) = s
makeDeleteMessage deleteLocs s@(ServerMessage _ _ (PayloadCL cells)) = ServerMessage Delete Success payload
  where locsCells = zip (map cellLocation cells) cells
        cells'    = map snd $ filter (\(l, _) -> not $ rangeContainsIndex deleteLocs l) locsCells
        payload   = PayloadDelete deleteLocs cells'
        -- remove the sels from the update that we know are blank from the deleted locs

changeMessageAction :: ASAction -> ASServerMessage -> ASServerMessage
changeMessageAction a (ServerMessage _ r p) = ServerMessage a r p

----------------------------------------------------------------------------------------------------------------------------------------------
-- Error Handling

-- | Not fully implemented yet
generateErrorMessage :: ASExecError -> String
generateErrorMessage e = case e of
  CircularDepError circDepLoc -> "Circular dependecy detected in cell " ++ (indexToExcel circDepLoc)
  (DBNothingException _)      -> "Unable to fetch cells from database."
  ExpressionNotEvaluable      -> "Expression not does not contain evaluable statement."
  ExecError                   -> "Error while evaluating expression."
  SyntaxError                 -> "Syntax error."
  _                           -> show e


----------------------------------------------------------------------------------------------------------------------------------------------
-- Time

getTime :: IO String
getTime = do 
  t <- getCurrentTime
  return $ take 23 $ show t

printWithTime :: String -> IO ()
printWithTime str = do
  time <- getTime
  let disp = "[" ++ time ++ "] " ++ str
  putStrLn (truncated disp)
  logDir <- getServerLogDir
  appendFile' (logDir ++ "console_log") ('\n':disp)

appendFile' :: String -> String -> IO ()
appendFile' fname msg = catch (appendFile fname msg) (\e -> putStrLn $ ("Error writing to log: " ++ show (e :: SomeException)))

writeToLog :: String -> CommitSource -> IO ()
writeToLog str (sid, uid) = do 
  -- first, write to master to log
  logDir <- getServerLogDir
  let sid' = T.unpack sid
      uid' = T.unpack uid
      loggedStr = '\n':str ++ "\n" ++ sid' ++ "\n" ++ uid'
      logPath = logDir ++ "server_log"
  appendFile' logPath loggedStr
  -- then write to individual log for the sheet
  let logPath' = logPath ++ sid'
  appendFile' logPath' loggedStr

-- can probably refactor with writeToLog to reduce code duplication
logBugReport :: String -> CommitSource -> IO ()
logBugReport str (sid, uid) = do 
  logDir <- getServerLogDir
  time <- getTime
  let sid' = T.unpack sid
      uid' = T.unpack uid
      loggedStr = '\n':str ++ "\n# " ++ sid' ++ "\n# " ++ uid'
      bugLogPath = logDir ++ "bug_reports"
  appendFile' bugLogPath loggedStr

writeErrToLog :: String -> CommitSource -> IO ()
writeErrToLog str (sid, uid) = do 
  logDir <- getServerLogDir
  time <- getTime
  let sid' = T.unpack sid
      uid' = T.unpack uid
      loggedStr = "\n#ERROR: "++ str ++ "\n# " ++ sid' ++ "\n# " ++ uid'
      logPath = logDir ++ "bug_reports"
  appendFile' logPath loggedStr
  -- then write to individual log for the sheet
  let logPath' = logPath ++ sid'
  appendFile' logPath' loggedStr

printWithTimeT :: String -> EitherTExec ()
printWithTimeT = lift . printWithTime

printObj :: (Show a) => String -> a -> IO ()
printObj disp obj = printWithTime (disp ++ ": " ++ (show $ seq () obj))
-- the seq is necessary so that the object gets evaluated before the time does in printWithTime. 

printObjT :: (Show a) => String -> a -> EitherTExec () 
printObjT disp obj = lift (printObj disp obj)

-- | For debugging purposes
printDebug :: (Show a) => String -> a -> IO ()
printDebug name obj = putStrLn ("\n\n" ++ name ++ ": " ++ (show $ seq () obj) ++ "\n\n")

printDebugT :: (Show a) => String -> a -> EitherTExec ()
printDebugT name obj = lift (printDebug name obj)

-- | Not yet implemented
getASTime :: IO ASTime
getASTime = return $ Time "hi" 1 2 3

----------------------------------------------------------------------------------------------------------------------------------------------
-- Id management

getUniqueId :: IO String
getUniqueId = return . U.toString =<< nextRandom

----------------------------------------------------------------------------------------------------------------------------------------------
-- viewing windows

intersectViewingWindow :: [ASCell] -> ASWindow -> [ASCell]
intersectViewingWindow cells vw = filter (inViewingWindow vw) cells
  where
    inViewingWindow :: ASWindow -> ASCell -> Bool
    inViewingWindow (Window wSheetId (tlc, tlr) (brc, brr)) (Cell (Index cSheetId (col,row)) _ _ _) = ((wSheetId==cSheetId) && (inRange col tlc (brc-tlc)) && (inRange row tlr (brr-tlr)))
    inRange :: Int -> Int -> Int -> Bool
    inRange elem start len = ((elem >= start) && (elem <= (start + len)))

-- new function, so that we don't have to do the extra filter/lookup by using just one
intersectViewingWindowLocs :: [ASIndex] -> ASWindow -> [ASIndex]
intersectViewingWindowLocs locs vw = filter (inViewingWindow vw) locs
  where
    inViewingWindow :: ASWindow -> ASIndex -> Bool
    inViewingWindow (Window wSheetId (tlc, tlr) (brc, brr)) (Index cSheetId (col,row)) =
      ((wSheetId==cSheetId) && (inRange col tlc (brc-tlc)) && (inRange row tlr (brr-tlr)))
    inRange :: Int -> Int -> Int -> Bool
    inRange elem start len = ((elem >= start) && (elem <= (start + len)))

updateWindow :: ASWindow -> ASUserClient -> ASUserClient
updateWindow w (UserClient uid conn _ sid) = UserClient uid conn w sid

-- | Computes set difference of window 2 minus window 1. Represented as a union of ASRange's. 
getScrolledLocs :: ASWindow -> ASWindow -> [ASRange]
getScrolledLocs (Window _ (-1,-1) (-1,-1)) (Window sheetid tl br) = [(Range sheetid (tl, br))]
getScrolledLocs w1@(Window _ (y,x) (y2,x2)) w2@(Window sheetid tl@(y',x') br@(y2',x2'))
  | windowsIntersect w1 w2 = getUncoveredLocs sheetid overlapping (tl, br)
  | otherwise = [(Range sheetid (tl, br))]
    where 
      overlapping = ((max y y', max x x'), (min y2 y2', min x2 x2'))
      windowsIntersect (Window _ (y,x) (y2,x2)) (Window _ (y',x') (y2',x2'))
            | y2 > y' = False 
            | y < y2' = False
            | x2 < x' = False 
            | x > x2' = False
            | otherwise = True 
            
getUncoveredLocs :: ASSheetId -> (Coord, Coord) -> (Coord, Coord) -> [ASRange]
getUncoveredLocs sheet (tlo, bro) (tlw, brw) = [Range sheet corners | corners <- cs]
    where
      trw = (col brw, row tlw)
      blw = (col tlw, row brw)
      tro = (col bro, row tlo)
      blo = (col tlo, row bro)
      cs = [(tlw, tro), (trw, bro), (brw, blo), (blw, tlo)]

----------------------------------------------------------------------------------------------------------------------------------------------
-- metrics

rectsIntersect :: Rect -> Rect -> Bool
rectsIntersect ((y,x),(y2,x2)) ((y',x'),(y2',x2'))
  | y2 < y' = False 
  | y > y2' = False
  | x2 < x' = False 
  | x > x2' = False
  | otherwise = True 

rangeDiff :: (Coord, Coord) -> Dimensions
rangeDiff (a,b) = (col b - col a + 1, row b - row a + 1)

reshapeColArr :: [a] -> Dimensions -> [[a]]
reshapeColArr lst@(x:xs) (m,n) = 
  if (length lst) /= (m*n-m)
    then (every m lst):(reshapeColArr xs (m,n))
    else []

----------------------------------------------------------------------------------------------------------------------------------------------
-- Cells

mergeCells :: [ASCell] -> [ASCell] -> [ASCell]
mergeCells c1 c2 = L.unionBy isColocated c1 c2

mergeCommits :: ASCommit -> ASCommit -> ASCommit
mergeCommits (Commit b a bd ad _) (Commit b' a' bd' ad' t) = Commit b'' a'' bd'' ad'' t
  where
    b'' = mergeCells b' b
    a'' = mergeCells a' a
    bd'' = L.unionBy hasSameKey bd' bd
    ad'' = L.unionBy hasSameKey ad' ad
    hasSameKey d1 d2 = (rangeDescriptorToKey d1) == (rangeDescriptorToKey d2)

rangeDescriptorToKey :: RangeDescriptor -> RangeKey
rangeDescriptorToKey desc = case desc of 
  ListDescriptor key -> key
  ObjectDescriptor key _ _ -> key

-- | Returns a list of blank cells at the given locations. For now, the language doesn't matter, 
-- because blank cells sent to the frontend don't get their languages saved. 
blankCellAt :: ASIndex -> ASCell
blankCellAt l = Cell l (Expression "" Excel) NoValue []

blankCellsAt :: [ASIndex] -> [ASCell]
blankCellsAt = map blankCellAt

removeCell :: ASIndex -> [ASCell] -> [ASCell]
removeCell idx = filter (((/=) idx) . cellLocation)

isMemberOfSpecifiedRange :: RangeKey -> ASCell -> Bool
isMemberOfSpecifiedRange key cell = case (cellExpression cell) of 
  Coupled _ _ _ key' -> key == key'
  _ -> False

---- partitions a set of cells into (cells belonging to one of the specified ranges, other cells)
partitionByRangeKey :: [ASCell] -> [RangeKey] -> ([ASCell], [ASCell])
partitionByRangeKey cells [] = ([], cells)
partitionByRangeKey cells keys = liftListTuple $ map (go cells) keys
  where go cs k = L.partition (isMemberOfSpecifiedRange k) cs

isString :: ASValue -> Bool
isString (ValueS _) = True
isString _ = False


----------------------------------------------------------------------------------------------------------------------------------------------
-- Locations

refToIndices :: ASReference -> Maybe [ASIndex]
refToIndices loc = case loc of
  IndexRef ind -> Just [ind]
  RangeRef r -> Just $ rangeToIndices r
  OutOfBounds -> Nothing

getHeight :: ASReference -> Int
getHeight (IndexRef _) = 1
getHeight (RangeRef (Range _ ((_,b),(_,d)))) = d-b+1

getWidth :: ASReference -> Int
getWidth (IndexRef _) = 1
getWidth (RangeRef (Range _ ((a,_),(c,_)))) = c-a+1

isRange :: ASReference -> Bool
isRange (IndexRef _) = False
isRange (RangeRef _) = True

-- tail recursive for speed
containsRange :: [ASReference] -> Bool
containsRange [] = False
containsRange (ref:refs) = case ref of
  (RangeRef _) -> True
  _ -> containsRange refs

rangeContainsRect :: ASRange -> Rect -> Bool
rangeContainsRect (Range _ ((x,y),(x2,y2))) ((x',y'),(x2',y2')) = tl && br
  where
    tl = (x' >= x) && (y' >= y)
    br = (x2' <= x2) && (y2' <= y2)

rangeToIndices :: ASRange -> [ASIndex]
rangeToIndices (Range sheet (ul, lr)) = [Index sheet (x,y) | x <- [startx..endx], y <- [starty..endy] ]
  where
    startx = min (fst ul) (fst lr)
    endx = max (fst ul) (fst lr)
    starty = min (snd ul) (snd lr)
    endy = max (snd ul) (snd lr)

rangeContainsIndex :: ASRange -> ASIndex -> Bool
rangeContainsIndex (Range sid1 ((x1,y1),(x2,y2))) (Index sid2 (x,y)) = and [ 
  sid1 == sid2, x >= x1, x <= x2, y >= y1, y <= y2 ]

rangeContainsRange :: ASRange -> ASRange -> Bool
rangeContainsRange (Range sid1 ((x1, y1), (x2, y2))) (Range sid2 ((x1', y1'), (x2', y2'))) = and [ 
  sid1 == sid2, x1 <= x1', x2 >= x2', y1 <= y1', y2 >= y2']

-- Probably needs case for columns
rangeContainsRef :: ASRange -> ASReference -> Bool
rangeContainsRef r ref = case ref of 
  IndexRef i  -> rangeContainsIndex r i
  RangeRef r' -> rangeContainsRange r r'
  OutOfBounds -> False 

orientRange :: ASRange -> ASRange
orientRange (Range sid (tl, br)) = Range sid (tl',br')
  where
    tl' = (min (col tl) (col br), min (row tl) (row br))
    br' = (max (col tl) (col br), max (row tl) (row br))

rangeToIndicesRowMajor :: ASRange -> [ASIndex]
rangeToIndicesRowMajor (Range sheet (ul, lr)) = [Index sheet (x,y) | y <- [starty..endy],x <- [startx..endx] ]
  where
    startx = min (fst ul) (fst lr)
    endx = max (fst ul) (fst lr)
    starty = min (snd ul) (snd lr)
    endy = max (snd ul) (snd lr)

matchSheets :: [ASWorkbook] -> [ASSheet] -> [WorkbookSheet]
matchSheets ws ss = [WorkbookSheet (workbookName w) (catMaybes $ lookupSheets w ss) | w <- ws]
  where lookupSheets workbook sheets = map (\sid -> lookupLambda sheetId sid sheets) (workbookSheets workbook)


shiftLoc :: Offset -> ASReference -> ASReference
shiftLoc (dy, dx) (IndexRef (Index sh (y,x))) = IndexRef $ Index sh (y+dy, x+dx)
shiftLoc (dy, dx) (RangeRef (Range sh ((y,x),(y2,x2)))) = RangeRef $ Range sh ((y+dy, x+dx), (y2+dy, x2+dx))

shiftInd :: Offset -> ASIndex -> ASIndex
shiftInd (dy, dx) (Index sh (y,x)) = Index sh (y+dy, x+dx)

getTopLeft :: ASRange -> ASIndex
getTopLeft (Range sh (tl,_)) = Index sh tl

getRangeDims :: ASRange -> Dimensions
getRangeDims (Range _ ((y1, x1), (y2, x2))) = (1 + abs (y2 - y1), 1 + abs (x2 - x1))

-- | The offset needed to translate the top left corner of the first range to get the 
-- top left corner of the second. 
getRangeOffset :: ASRange -> ASRange -> Offset
getRangeOffset r1 r2 = getIndicesOffset (getTopLeft r1) (getTopLeft r2)

getCopyOffSets :: ASRange -> ASRange -> [Offset]
getCopyOffSets from to = offsets
  where
    (fromYDim, fromXDim) = getRangeDims from
    (toYDim, toXDim) = getRangeDims to
    yRep = max 1 (toYDim `div` fromYDim)
    xRep = max 1 (toXDim `div` fromXDim)
    (topYOffset, topXOffset) = getRangeOffset from to
    yRepOffsets = take yRep [0,fromYDim..]
    xRepOffsets = take xRep [0,fromXDim..]
    offsets = [(topYOffset + y, topXOffset + x) | y <- yRepOffsets, x <- xRepOffsets]


getIndicesOffset :: ASIndex -> ASIndex -> Offset
getIndicesOffset (Index _ (y, x)) (Index _ (y', x')) = (y'-y, x'-x)

pointerToIndex :: ASIndex -> ASIndex
pointerToIndex idx = case idx of 
  Index _ _ -> idx
  Pointer sid coord -> Index sid coord

indexToPointer :: ASIndex -> ASIndex
indexToPointer idx = case idx of 
  Index sid coord -> Pointer sid coord
  Pointer _ _ -> idx
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

----------------------------------------------------------------------------------------------------------------------------------------------
-- Converting between Excel and AlphaSheets formats

-- "AA" -> 27
colStrToInt :: String -> Int
colStrToInt "" = 0
colStrToInt (c:cs) = 26^(length(cs)) * coef + colStrToInt cs
  where
    coef = fromJust (L.elemIndex (C.toUpper c) ['A'..'Z']) + 1

-- 27 -> "AA",  218332954 ->"RITESH"
intToColStr :: Int -> String
intToColStr x
  | x <= 26 = [['A'..'Z'] !! (x-1)]
  | otherwise = intToColStr d ++ [['A'..'Z'] !! m]
      where
        m = (x-1) `mod` 26
        d = (x-1) `div` 26

-- used in DB Ranges
indexToExcel :: ASIndex -> String
indexToExcel (Index _ (c,r)) = (intToColStr c) ++ (show r)

----------------------------------------------------------------------------------------------------------------------
-- Errors

execErrorToValueError :: ASExecError -> ASValue
execErrorToValueError e = ValueError (show e) "Exec error"

----------------------------------------------------------------------------------------------------------------------
-- Testing

testLocs :: Int -> [ASIndex]
testLocs n = [Index "" (i,1) | i <-[1..n]]

testCells :: Int -> [ASCell]
testCells n =  L.map (\l -> Cell (Index "" (l,1)) (Expression "hi" Python) (ValueS "Str") []) [1..n]