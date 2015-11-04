module AS.Util where

import AS.Types.Core

import Prelude
import System.Directory
import System.FilePath.Posix
import Data.Time.Clock
import Data.Maybe (isNothing,fromJust)
import Data.UUID.V4 (nextRandom)
import Text.ParserCombinators.Parsec

import Data.Aeson hiding (Success)
import Control.DeepSeq
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

import Data.Ord

import Debug.Trace 

-------------------------------------------------------------------------------------------------------------------------
-- For debugging purposes only 

trace' :: (Show a) => String -> a -> a
trace' s x = trace (s ++ (show x)) x

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
isEmptyCell = ((==) "") . expression . cellExpression

liftEitherTuple :: Either b (a0, a1) -> (Either b a0, Either b a1)
liftEitherTuple (Left b) = (Left b, Left b)
liftEitherTuple (Right (a0, a1)) = (Right a0, Right a1)

liftListTuple :: [([a],[b])] -> ([a], [b])
liftListTuple t = (concat $ map fst t, concat $ map snd t)

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy delimiter = foldr f [[]]
  where f c l@(x:xs) | c == delimiter = []:l
                     | otherwise = (c:x):xs

catchEitherT :: EitherTExec ASValue -> EitherTExec ASValue
catchEitherT a = do
  result <- liftIO $ catch (runEitherT a) whenCaught
  case result of
    (Left e) -> left e
    (Right e) -> right e
    where whenCaught = (\e -> return . Right $ ValueError (show e) "StdErr" "" 0) :: (SomeException -> IO (Either ASExecError ASValue))

fromDouble :: Double -> Either Double Int
fromDouble x = if (x == xInt)
  then Right $ fromInteger (round x)
  else Left x
  where xInt = fromInteger (round x)
--------------------------------------------------------------------------------------------------------------
-- Lists

-- assumes all rows have same length, and every input ASValue is a row (i.e. column-major)
-- TODO deal with case of RDataFrame
transposeList :: [ASValue] -> [ASValue]
transposeList l = case (head l) of
  (ValueL _) -> map ValueL $ L.transpose $ map toList l
  _ -> [ValueL l]

isHighDimensional :: Int -> ASValue -> Bool
isHighDimensional depth (ValueL l) = if (depth + 1 > 2)
  then True
  else isHighDimensional (depth + 1) (head l)
isHighDimensional depth (RDataFrame l) = if (depth + 1 > 2)
  then True
  else isHighDimensional (depth + 1) (head l)
isHighDimensional depth _ = False

sanitizeList :: ASValue -> ASValue
sanitizeList v = if (isHighDimensional 0 v)
  then ValueError "Cannot embed lists of dimension > 2." "StdErr" "" 0
  else v

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
makeGetMessage cells = changeActionToGet $ makeUpdateMessage (Right cells)
  where changeActionToGet (ServerMessage _ r p) = ServerMessage Get r p        

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

serverLogPath :: IO String
serverLogPath = do
  mainDir <- getCurrentDirectory
  return $ mainDir </> "logs/server_log"

printWithTime :: String -> IO ()
printWithTime str = do
  time <- getTime
  let disp = "[" ++ time ++ "] " ++ str
  putStrLn (truncated disp)

appendFile' :: String -> String -> IO ()
appendFile' fname msg = catch (appendFile fname msg) (\e -> putStrLn $ ("Error writing to log: " ++ show (e :: SomeException)))  

writeToLog :: String -> CommitSource -> IO ()
writeToLog str (sid, uid) = do 
  -- first, write to master to log
  let sid' = T.unpack sid
      uid' = T.unpack uid
      loggedStr = '\n':str ++ "\n# SHEET_ID: " ++ sid' ++ "\n# USER_ID: " ++ uid'
  serverLog <- serverLogPath
  appendFile' serverLog loggedStr
  -- then write to individual log for the sheet
  let serverLog' = serverLog ++ sid'
  appendFile' serverLog' ('\n':str)

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
    where overlapping = ((max y y', max x x'), (min y2 y2', min x2 x2'))
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
-- Cells

isListMember :: ASCell -> Bool
isListMember (Cell _ _ _ ts) = any id $ map (\t -> case t of
  (ListMember _) -> True
  _ -> False) ts

mergeCells :: [ASCell] -> [ASCell] -> [ASCell]
mergeCells c1 c2 = L.unionBy isColocated c1 c2

blankCellsAt :: ASLanguage -> [ASIndex] -> [ASCell]
blankCellsAt lang = map (\l -> Cell l (Expression "" lang) NoValue [])

removeCell :: ASIndex -> [ASCell] -> [ASCell]
removeCell idx = filter (((/=) idx) . cellLocation)

isMemberOfSpecifiedList :: ListKey -> ASCell -> Bool
isMemberOfSpecifiedList key cell = case (getListTag cell) of
  (Just (ListMember key')) -> key' == key
  Nothing -> False

-- partitions a set of cells into (cells belonging to one of the specified lists, other cells)
partitionByListKeys :: [ASCell] -> [ListKey] -> ([ASCell], [ASCell])
partitionByListKeys cells [] = ([], cells)
partitionByListKeys cells keys = liftListTuple $ map (partitionByListKey cells) keys
  where
    partitionByListKey cs k = L.partition (isMemberOfSpecifiedList k) cs

listKeyOrdering :: ListKey -> ListKey -> Ordering
listKeyOrdering k1 k2 = if (k1 == k2)
  then EQ
  else if (k1 > k2)
    then GT
    else LT

isString :: ASValue -> Bool
isString (ValueS _) = True
isString _ = False

----------------------------------------------------------------------------------------------------------------------------------------------
-- References in maps

shouldGroupRefs :: (ASCell, [ASReference]) -> Bool
shouldGroupRefs (c, refs) = case (language $ cellExpression c) of
  R -> containsRange refs
  _ -> False

groupRef :: ASLanguage -> (ASRange, [ASValue]) -> Maybe (ASReference, ASValue)
groupRef lang (ref@(Range _ ((c1,r1),(c2,r2))), vals) = case lang of
  R -> Just (RangeRef ref, RDataFrame vals')
    where
      rows = chunksOf (r2-r1+1) vals
      vals' = map ValueL rows
  _ -> Nothing

formatValuesForMap :: [(ASIndex, Maybe ASCell)] -> [(ASReference, ASValue)]
formatValuesForMap pairs = formattedPairs
  where formattedPairs = map (\(l, c) -> (IndexRef l, getSanitizedCellValue c)) pairs

getSanitizedCellValue :: Maybe ASCell -> ASValue
getSanitizedCellValue c = case c of
  Just cell -> cellValue cell
  Nothing -> NoValue

----------------------------------------------------------------------------------------------------------------------------------------------
-- Locations

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

getHeight :: ASReference -> Int
getHeight (IndexRef (Index _ _)) = 1
getHeight (RangeRef (Range _ ((_,b),(_,d)))) = d-b+1

getWidth :: ASReference -> Int
getWidth (IndexRef (Index _ _)) = 1
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

getPasteOffsets :: ASRange -> ASRange -> [Offset]
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


getIndicesOffset :: ASIndex -> ASIndex -> Offset
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

containsDFMember :: [Maybe ASCell] -> Bool
containsDFMember = any (\c -> case c of
  Just cell -> isDFMember cell
  Nothing -> False)

isDFMember :: ASCell -> Bool
isDFMember (Cell _ _ _ ts) = any (\t -> case t of
  DFMember -> True
  _ -> False) ts

getListKeyUnsafe :: ASCell -> ListKey
getListKeyUnsafe cell = listKey
  where (Just (ListMember listKey)) = getListTag cell

getListTag :: ASCell -> Maybe ASCellTag
getListTag (Cell _ _ _ ts) = L.find (\t -> case t of
  (ListMember _) -> True
  _ -> False) ts

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
-- Testing

testLocs :: Int -> [ASIndex]
testLocs n = [Index "" (i,1) | i <-[1..n]]

testCells :: Int -> [ASCell]
testCells n =  L.map (\l -> Cell (Index "" (l,1)) (Expression "hi" Python) (ValueS "Str") []) [1..n]

----------------------------------------------------------------------------------------------------------------------
-- Parsing

-- | Matches an escaped string and returns the unescaped version. E.g. if the input is: 
-- "\"hello"
-- quotedString would match this and return this output: 
-- "hello
quotedString :: Parser String
quotedString = (quoteString <|> apostropheString)
  where
    quoteString      = quotes $ many $ escaped <|> noneOf ['"']
    apostropheString = apostrophes $ many $ escaped <|> noneOf ['\'']
    quotes           = between quote quote
    quote            = char '"' --
    apostrophes      = between apostrophe apostrophe
    apostrophe       = char '\'' -- TODO apostrophes also
    escaped          = char '\\' >> choice (zipWith escapedChar codes replacements)
    escapedChar code replacement = char code >> return replacement
    codes            = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '/']
    replacements     = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']