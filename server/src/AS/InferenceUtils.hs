module AS.InferenceUtils where

import qualified Data.List.Split as LS
import AS.Parsing.Out (showValue)
import AS.Parsing.In (parseValue)
import qualified Data.List as L
import Data.Maybe
import AS.Types.Core
import AS.DB.API                as DB
import AS.Parsing.Substitutions as S
import AS.Util


type Position = (Int,Int)
type NumberedCell = (Position,ASCell)
type PatternGroup = [NumberedCell]
type Pattern = ([NumberedCell],(Int -> ASValue))

-- Deal with offsets and positions

getAbsoluteDragPositions :: ASRange -> ASRange -> Position -> [Offset]
getAbsoluteDragPositions (Range _ ((a,b),(c,d))) (Range _ ((a',b'),(c',d'))) (x,y) 
  | (a==a') && (b==b') && (d==d') = takeWhile (\(i,_) -> i<=c') $ zip [x,x+(c-a+1)..] (repeat y)
  | (c==c') && (d==d') && (b==b') = takeWhile (\(i,_) -> i>=a') $ zip [x,x-(c-a+1)..] (repeat y)
  | (a==a') && (b==b') && (c==c') = takeWhile (\(_,j) -> j<=d') $ zip (repeat x) [y,y+(d-b+1)..]
  | (c==c') && (d==d') && (a==a') = takeWhile (\(_,j) -> j>=b') $ zip (repeat x) [y,y-(d-b+1)..]

getDragOffsets :: ASRange -> ASRange -> Position -> [Offset]
getDragOffsets r1 r2 (x,y) = map (\(a,b) -> (a-x,b-y)) $ getAbsoluteDragPositions r1 r2 (x,y)

dragRightOrDown :: ASRange -> ASRange -> Bool
dragRightOrDown (Range _ ((a,b),(c,d))) (Range _ ((a',b'),(c',d'))) 
  | (a==a') && (b==b') && (d==d') = True
  | (c==c') && (d==d') && (b==b') = False
  | (a==a') && (b==b') && (c==c') = True
  | (c==c') && (d==d') && (a==a') = False

isHorizontal :: ASRange -> ASRange -> Bool
isHorizontal (Range _ ((a,b),(c,d))) (Range _ ((a',b'),(c',d'))) 
  | (a==a') && (b==b') && (d==d') = True
  | (c==c') && (d==d') && (b==b') = True
  | (a==a') && (b==b') && (c==c') = False
  | (c==c') && (d==d') && (a==a') = False

getNumberedCells :: ASRange -> ASRange -> IO [[NumberedCell]]
getNumberedCells r1@(Range _ ((a,b),(c,d))) r2
  | (isHorizontal r1 r2) = do 
    cells <-  DB.getCells $ rangeToIndicesRowMajor r1
    putStrLn $ "db cells 1 " ++ show cells
    let rectCells = formatRect (c-a+1) cells 
    let positions = formatRect (c-a+1) [(x,y)| y<-[b..d], x<-[a..c]]
    let nCells = map (\(l1,l2) -> zip l1 l2) $ zip positions rectCells
    return $ filterMaybeNumCells nCells
  | otherwise = do 
    cells <- DB.getCells $ rangeToIndices r1
    putStrLn $ "db cells 2 " ++ show cells
    let rectCells = formatRect (d-b+1) cells
    putStrLn $ "rect cells " ++ show rectCells
    let positions = formatRect (d-b+1) [(x,y)| x<-[a..c], y<-[b..d]]
    putStrLn $ "positions " ++ show positions
    let nCells = map (\(l1,l2) -> zip l1 l2) $ zip positions rectCells
    putStrLn $ "n cells maybe " ++ show nCells
    return $ filterMaybeNumCells nCells

formatRect :: Int -> [a] -> [[a]]
formatRect i [] = []
formatRect i lst = [take i lst] ++ (formatRect i (drop i lst))

filterMaybeNumCells :: [[(Position,Maybe ASCell)]] -> [[NumberedCell]]
filterMaybeNumCells mCells = map (map (\(p,Just c) -> (p,c))) cells
  where
    cells = map (filter (\(p,c) -> isJust c)) mCells

-- Deal with formula cells

isFormulaCell :: NumberedCell -> Bool
isFormulaCell (pos,cell) = parsedVal /= (Right (cellValue cell))
  where
    lang = language $ cellExpression cell
    xp = expression $ cellExpression cell
    parsedVal = parseValue lang xp

extractFormulaCells :: [[NumberedCell]] -> [NumberedCell]
extractFormulaCells cells = concat $ map (filter isFormulaCell) cells

getMappedFormulaCells :: ASRange -> ASRange -> [[NumberedCell]] -> [ASCell]
getMappedFormulaCells r1 r2 cells = concat $ map translateCell formulaCells
  where
    formulaCells = trace' "FORMULA CELLS "  $ extractFormulaCells cells
    translateCell (pos,fc) = map (\offset -> S.shiftCell offset fc) $ (trace' "OFFSETS " (getDragOffsets r1 r2 pos))

-- Deal with patterns

extractPatternGroups :: [[NumberedCell]] -> [PatternGroup]
extractPatternGroups cells = concat $ map (LS.splitWhen isFormulaCell) cells

getMappedPatternGroups :: ASRange -> ASRange -> [[NumberedCell]] -> [ASCell]
getMappedPatternGroups r1 r2 cells = concat $ map (translatePatternGroupCells r1 r2) patternGroups
  where
    patternGroups = trace' "PATTERN GROUPS " $ extractPatternGroups cells

translatePatternGroupCells :: ASRange -> ASRange -> PatternGroup -> [ASCell]
translatePatternGroupCells r1 r2 pg = cells
  where
    patterns = decomposePatternGroup pg
    cells = concat $ map (translatePatternCells r1 r2) patterns

decomposePatternGroup :: PatternGroup -> [Pattern]
decomposePatternGroup [] = []
decomposePatternGroup pg = patterns
  where
    searchOrder = reverse $ L.inits $ (trace' "INPUT " pg)
    len = length pg
    lengths = [len,(len-1)..0]
    searchItems =  zip searchOrder lengths
    patternSplit = trace' "PATTERN SPLIT " $ L.find (\(x,_) -> isJust (getPattern x)) searchItems
    patterns = if isNothing patternSplit
      then [] 
      else largestStartPattern : (decomposePatternGroup restOfPatternGroup)
        where
          ps = fromJust patternSplit
          largestStartPattern = fromJust $ getPattern $ fst ps
          restOfPatternGroup = drop (snd ps) pg

translatePatternCells :: ASRange -> ASRange -> Pattern -> [ASCell]
translatePatternCells r1 r2 pattern = concat $ map translatePatternCell indexCells
  where
    len = length (fst pattern)
    indexCells = zip (fst pattern) [0..(len-1)]
    translatePatternCell ((pos,cell),ind) = newCells
      where
        newPositions = getAbsoluteDragPositions r1 r2 pos
        num = length newPositions
        seriesIndices = if (dragRightOrDown r1 r2)
          then [ind,(ind+len)..(ind+(num-1)*len)]
          else [ind,(ind-len)..(ind-(num-1)*len)]
        lang = language $ cellExpression cell
        newVals = map (snd pattern) seriesIndices
        newLocs = map (Index (rangeSheetId r1)) newPositions
        newExpressions = map (\v -> Expression (showValue lang v) lang) newVals
        newCells = map (\(l,e,v) -> Cell l e v (cellTags cell)) $ zip3 newLocs newExpressions (trace' "NEW VALS " newVals)

-- deal with pattern matching

type PatternMatcher = [NumberedCell] -> Maybe Pattern

isInt :: ASValue -> Maybe Int
isInt (ValueI i) = Just i
isInt _ = Nothing

getPattern :: [NumberedCell] -> Maybe Pattern
getPattern c = if noMatch
  then Nothing
  else Just (head patterns)
  where
    maybePatterns = map (\f -> f c) patternMatchers
    patterns = catMaybes maybePatterns
    noMatch = (length patterns == 0)

patternMatchers :: [PatternMatcher]
patternMatchers = [arithSeries,trivialMatcher,emptyMatcher]

trivialMatcher :: PatternMatcher
trivialMatcher [c] = Just $ ([c],\n -> cellValue (snd c))
trivialMatcher _ = Nothing


emptyMatcher :: PatternMatcher
emptyMatcher _ = Nothing

getValsFromNumCells :: [NumberedCell] -> [ASValue]
getValsFromNumCells c = map cellValue $ map snd c

arithSeries :: PatternMatcher
arithSeries cells = result 
  where
    vals = getValsFromNumCells cells
    ints = map isInt vals
    result = if (any isNothing ints)
      then Nothing
      else p
        where
          series = map (\(ValueI i) -> i) vals
          p = if (isArithmSeq series)
            then Just $ (cells,\i -> ValueI ((series!!0) + i*(series!!1-series!!0)))
            else Nothing

isArithmSeq :: (Eq a, Num a) => [a] -> Bool
isArithmSeq [] = False
isArithmSeq [x] = False
isArithmSeq [x,y] = True
isArithmSeq (x:y:z:xs) = (x - y) == (y - z) && isArithmSeq (y:z:xs)