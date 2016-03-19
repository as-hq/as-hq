{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module AS.InferenceUtils where

import qualified Data.List.Split as LS
import qualified Data.List as L
import Data.Maybe hiding (fromJust)
import Data.Char
import Control.Lens hiding (index)
import Database.Redis (Connection)
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)

import AS.Prelude
import Prelude()
import AS.Types.Excel
import AS.Types.Formats
import AS.Types.Cell
import AS.Types.Eval

import AS.Parsing.Show (showPrimitive)
import AS.Parsing.Read (parseValue)
import AS.DB.API                as DB
import AS.Parsing.Substitutions as S
import AS.Util
import AS.Kernels.Excel.Compiler hiding (pos)
import AS.Kernels.Excel.Util

type Position = Coord
type PatternGroup = [ASCell]
type Pattern = ([ASCell],(Int -> ASValue))

pos :: ASCell -> Position
pos = view (cellLocation.index)

------------------------------------------------------------------------------------------------------------------
-- Deal with offsets and positions

-- Given the sel and drag ranges, and the current position, return all the corresponding positions in the drag rng 
-- Ex selRng A1:A3 and drag range A1:A6, pos (1,2) -> absolute positions (1,2) and (1,5)
-- Only works on finite ranges.
getFiniteAbsoluteDragPositions :: ASRange -> ASRange -> Position -> [Position]
getFiniteAbsoluteDragPositions (Range _ (rangeCoord1, extendedRangeCoord2)) (Range _ (rangeCoord1', extendedRangeCoord2')) pos = positions
  where 
    rangeCoord2 = fromExtendedCoord extendedRangeCoord2
    rangeCoord2' = fromExtendedCoord extendedRangeCoord2'
    positions = map(\(x,y) -> makeCoord x y) positionPairs
    positionPairs
      | (a==a') && (b==b') && (d==d') = takeWhile (\(i,_) -> i<=c') $ zip [x,x+(c-a+1)..] (repeat y)
      | (c==c') && (d==d') && (b==b') = takeWhile (\(i,_) -> i>=a') $ zip [x,x-(c-a+1)..] (repeat y)
      | (a==a') && (b==b') && (c==c') = takeWhile (\(_,j) -> j<=d') $ zip (repeat x) [y,y+(d-b+1)..]
      | otherwise = takeWhile (\(_,j) -> j>=b') $ zip (repeat x) [y,y-(d-b+1)..]
    a  = rangeCoord1^.col
    b  = rangeCoord1^.row
    c  = rangeCoord2^.col
    d  = rangeCoord2^.row
    a' = rangeCoord1'^.col
    b' = rangeCoord1'^.row
    c' = rangeCoord2'^.col
    d' = rangeCoord2'^.row
    x  = pos^.col
    y  = pos^.row

-- Same as above, but only return the offsets and not the absolute positions
getDragOffsets :: ASRange -> ASRange -> Position -> [Offset]
getDragOffsets r1 r2 coord =
  map (\c -> Offset (c^.col-coord^.col) (c^.row-coord^.row)) $ getFiniteAbsoluteDragPositions r1 r2 coord

-- Given the sel range and drag range, return the cells in the sel range by DB lookup
-- If the selection was horizontal, row-major, else column major
-- Directionality matters; if drag left, each row is from right to left
-- Inference ignores empty cells, so they can safely be filtered out here
getCellsRect :: Connection -> ASRange -> ASRange -> IO [[ASCell]]
getCellsRect conn r1@(Range _ (coord1,coord2)) r2@(Range _ (coord1',coord2')) =  fmap filterMaybeNumCells rectCells
  where
    rectCells 
      | (a==a') && (b==b') && (d==d') = do 
        cells <- DB.getCells conn $ finiteRangeToIndices r1
        return $ formatRect (c-a+1) cells 
      | (c==c') && (d==d') && (b==b') = do 
        cells <- DB.getCells conn $ finiteRangeToIndices r1
        return $ map reverse $ formatRect (c-a+1) cells
      | (a==a') && (b==b') && (c==c') = do 
        cells <- DB.getCells conn $ finiteRangeToIndices r1
        return $ formatRect (d-b+1) cells
      | otherwise = do 
        cells <- DB.getCells conn $ finiteRangeToIndices r1
        return $ map reverse $ formatRect (d-b+1) cells
    a  = coord1 ^.col^.int
    b  = coord1 ^.row^.int
    c  = coord2 ^.finiteCol^.int
    d  = coord2 ^.finiteRow^.int
    a' = coord1'^.col^.int
    b' = coord1'^.row^.int
    c' = coord2'^.finiteCol^.int
    d' = coord2'^.finiteRow^.int


formatRect :: Int -> [a] -> [[a]]
formatRect i [] = []
formatRect i lst = [take i lst] ++ (formatRect i (drop i lst))

filterMaybeNumCells :: [[Maybe ASCell]] -> [[ASCell]]
filterMaybeNumCells mCells = map (map (\(Just c) -> c)) cells
  where
    cells = map (filter isJust) mCells

------------------------------------------------------------------------------------------------------------------
-- Deal with formula cells

-- Inference deals with formula and literal cells differently. Formula cells are mapped just like copy. 
-- A cell is a formula cell if its expression and value aren't the same
-- Excel isn't included in parseValue like the rest, so it has to be separately cased on
isFormulaCell :: ASCell -> Bool
isFormulaCell cell = not valExpEqual
  where
    lang = cell^.cellExpression.language
    xp = cell^.cellExpression.expression
    valExpEqual = case lang of
      Excel -> case maybeVal of
        Nothing  -> False
        Just val -> val^.orig == cell^.cellValue
        where
          formula = $fromRight $ eitherResult $ parse literal xp
          excelToASValue (Right (Basic (Var eValue))) = Just $ eValToASValue eValue
          excelToASValue _ = Nothing
          maybeVal = excelToASValue formula
      otherwise -> parseValue lang xp == Right (CellValue $ cell^.cellValue)

-- Given the 2D list of cells in the sel range, extract all formula cells
extractFormulaCells :: [[ASCell]] -> [ASCell]
extractFormulaCells cells = concatMap (filter isFormulaCell) cells

-- Given the sel range, drag range, and 2D list of sel range cells, return all cells corresponding to formula cells
-- (for each formula cell, do a copy-like operation to fill the drag range)
getMappedFormulaCells :: ASRange -> ASRange -> [[ASCell]] -> [ASCell]
getMappedFormulaCells r1 r2 cells = catMaybes $ concatMap translateCell formulaCells
  where
    formulaCells = extractFormulaCells cells
    translateCell c = map (\offset -> S.shiftCell offset c) $ (getDragOffsets r1 r2 (pos c))

------------------------------------------------------------------------------------------------------------------
-- Deal with patterns

-- A pattern group is a region of cells between adjacent formula cells
-- it may have many actual patterns within it
-- For example, 1,2,a,3,4 has three actual patterns (1,2) (a) (3,4)

-- By splitting between formula cells, extract all pattern groups
extractPatternGroups :: [[ASCell]] -> [PatternGroup]
extractPatternGroups cells = concatMap (LS.splitWhen isFormulaCell) cells

getMappedPatternGroups :: ASRange -> ASRange -> [[ASCell]] -> [ASCell]
getMappedPatternGroups r1 r2 cells = concatMap (translatePatternGroupCells r1 r2) patternGroups
  where
    patternGroups = extractPatternGroups cells

-- Given the sel and drag ranges, and the pattern group, return all the new cells it creates
translatePatternGroupCells :: ASRange -> ASRange -> PatternGroup -> [ASCell]
translatePatternGroupCells r1 r2 pg = cells
  where
    patterns = decomposePatternGroup pg
    cells = concatMap (translatePatternCells r1 r2) patterns

-- Decompose a pattern group into patterns
-- A pattern has a bunch of cells, and a function from term number -> ASValue
-- Find the largest prefix that matches a pattern, and then recurse
decomposePatternGroup :: PatternGroup -> [Pattern]
decomposePatternGroup [] = []
decomposePatternGroup pg = patterns
  where
    searchOrder = reverse $ L.inits pg
    len = length pg
    lengths = [len,(len-1)..0]
    searchItems =  zip searchOrder lengths
    patternSplit = L.find (\(x,_) -> isJust (getPattern x)) searchItems
    patterns = if isNothing patternSplit
      then [] 
      else largestStartPattern : (decomposePatternGroup restOfPatternGroup)
        where
          ps = $fromJust patternSplit
          largestStartPattern = $fromJust $ getPattern $ fst ps
          restOfPatternGroup = drop (snd ps) pg

-- Get the cells corresponding to a pattern using position offsets (expression = value)
translatePatternCells :: ASRange -> ASRange -> Pattern -> [ASCell]
translatePatternCells r1 r2 pattern = concatMap translatePatternCell indexCells
  where
    len = length (fst pattern)
    indexCells = zip (fst pattern) [0..(len-1)]
    translatePatternCell (cell,ind) = newCells
      where
        newPositions = getFiniteAbsoluteDragPositions r1 r2 (pos cell)
        num = length newPositions
        seriesIndices = [ind,(ind+len)..(ind+(num-1)*len)]
        lang = cell^.cellExpression.language
        newVals = map (snd pattern) seriesIndices
        newLocs = map (Index (rangeSheetId r1)) newPositions
        newExpressions = map (\v -> Expression (showPrimitive lang v) lang) newVals
        newCells = map (\(l,e,v) -> Cell l e v (cell^.cellProps) Nothing Nothing) $ zip3 newLocs newExpressions newVals

------------------------------------------------------------------------------------------------------------------
-- deal with the actual pattern matching (quite literally)
-- TODO: currently incomplete, need to get all of Excel eventually

type PatternMatcher = [ASCell] -> Maybe Pattern

-- Iterate through all pattern matchers in order and return the first one that matches the list of cells
getPattern :: [ASCell] -> Maybe Pattern
getPattern c = if noMatch
  then Nothing
  else Just ($head patterns)
  where
    maybePatterns = map (\f -> f c) patternMatchers
    patterns = catMaybes maybePatterns  
    noMatch = (length patterns == 0)

-- List of pattern matchers
patternMatchers :: [PatternMatcher]
patternMatchers = [sequenceMatcher,arithMatcher,trivialMatcher]

trivialMatcher :: PatternMatcher
trivialMatcher [c] = Just $ ([c],\n -> c^.cellValue)
trivialMatcher _ = Nothing

-- Solely for convenience in using the arithmetic operations in arithMatcher. If we end up 
-- needing something like this elsewhere too I'll consider moving it to Types.Core.
instance Num ASValue where
  negate (ValueI i) = ValueI (-i)
  negate (ValueD d) = ValueD (-d)
  signum (ValueI i) = ValueI $ signum i
  signum (ValueD d) = ValueD $ signum d
  abs (ValueD d) = ValueD (abs d)
  abs (ValueI i) = ValueI (abs i)
  (+) (ValueD d) (ValueD d') = ValueD (d+d')
  (+) (ValueI i) (ValueD d) = ValueD ((fromIntegral i)+d)
  (+) (ValueD d) (ValueI i) = ValueD ((fromIntegral i)+d)
  (+) (ValueI i) (ValueI i') = ValueI (i+i')
  (*) (ValueD d) (ValueD d') = ValueD (d*d')
  (*) (ValueI i) (ValueD d) = ValueD ((fromIntegral i)*d)
  (*) (ValueD d) (ValueI i) = ValueD ((fromIntegral i)*d)
  (*) (ValueI i) (ValueI i') = ValueI (i*i')
  fromInteger a = (ValueI (fromIntegral a))

toDouble :: ASValue -> Maybe Double
toDouble (ValueI i) = Just $ fromIntegral i
toDouble (ValueD d) = Just d
toDouble _ = Nothing

isArithmSeq :: [Double] -> Bool
isArithmSeq [] = False
isArithmSeq [x] = False
isArithmSeq [x,y] = True
isArithmSeq (x:y:z:xs) = (eqDouble (x - y) (y - z)) && isArithmSeq (y:z:xs)
  where eqDouble x y = (abs (x-y) < 1e-6) -- bad doubles comparison, but good enough for now

arithMatcher :: PatternMatcher
arithMatcher cells = result 
  where
    vals = map (view cellValue) cells
    mDoubles = map toDouble vals
    result = if (any isNothing mDoubles)
      then Nothing
      else p
        where
          p = if (isArithmSeq $ catMaybes mDoubles)
            then Just $ (cells, \i -> (vals!!0) + (fromInteger . fromIntegral $ i)*(vals!!1-vals!!0))
            else Nothing

lowercase :: ASValue -> ASValue
lowercase (ValueS s) = (ValueS (map toLower s))
lowercase v = v

-- sequences of strings
-- not matching arithmetic subsequences yet (mon,wed,...)

sequenceMatcher :: PatternMatcher
sequenceMatcher [] = Nothing -- don't match an empty pattern as a subsequence
sequenceMatcher cells = result
  where
    vals' = map (view cellValue) cells
    vals = map lowercase vals' -- match lowercase
    seqMatches = filter (\seq -> L.isInfixOf vals seq) sequences -- match infix (consecutive subsequence)
    result = if (length seqMatches == 0)
      then Nothing
      else Just (cells,valFunc)
        where
          seq = $head seqMatches
          startIndex = $fromJust $ L.findIndex ((==) ($head vals)) seq
          valFunc = \i -> seq !! ((startIndex+i) `mod` (length seq))

sequences :: [[ASValue]]
sequences = months ++ days

months :: [[ASValue]]
months = [v1,v2]
  where
    v1 = map ValueS ["jan","feb","mar","apr","may","jun","jul","aug","sept","oct","nov","dec"]
    v2 = map ValueS ["january","february","march","april","may","june","july","august","september","october","november","december"]

days :: [[ASValue]]
days = [v1,v2]
  where
    v1 = map ValueS ["mon","tue","wed","thu","fri","sat","sun"]
    v2 = map ValueS ["monday","tuesday","wednesday","thursday","friday","saturday","sunday"]
