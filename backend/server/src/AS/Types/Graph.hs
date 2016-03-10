
{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}

module AS.Types.Graph where

import Prelude()
import AS.Prelude

import AS.Types.Cell
import AS.Types.Commits
import AS.Types.Locations
import AS.Types.Eval
import AS.Types.EvalHeader
import AS.Types.CellProps
import AS.Types.Bar
import AS.Types.CondFormat
import AS.Types.User

import Text.Read (readMaybe)
import Debug.Trace

import Data.List.Split (splitOn)
import qualified Data.Text as T 
import qualified Data.List as L
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString               as B

import GHC.Generics
import Data.SafeCopy
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)

import AS.Serialize as S (encode)
import Control.Lens hiding (index, context)
----------------------------------------------------------------------------------------------------------------------------------------------
-- Graph queries

-- A relation (toLoc,[fromLoc]); a toLoc must be an index, a fromLoc can be any ancestor
type ASRelation = (ASIndex, [GraphAncestor])

-- Graph read (getX) requests
data GraphReadRequest =   GetDescendants [AncestryRequestInput] 
                        | GetImmediateDescendants [AncestryRequestInput] 
                        | GetProperDescendants [AncestryRequestInput] 
                        | GetImmediateAncestors [AncestryRequestInput] 
                        | GetAllAncestors [AncestryRequestInput] deriving (Show)

data AncestryRequestInput = IndexInput ASIndex | RangeInput ASRange deriving (Show)

-- Graph set (setX) requests
data GraphWriteRequest = SetRelations [ASRelation] | ClearAllDAGs | ClearSheetDAG ASSheetId deriving (Show)

-- Graph input for functions like getDescendants can be indexes or ranges. Getting the descendants 
-- of a range = descendants of decomposed indices in ranges
-- ::ALEX:: rename


-- The output of a graph descendant can only be an index (currently)
-- One can imagine in the future that there's a constant in A1 that gets dragged down in an absolute reference
-- In this case, that constant would have a lot of descendants, and it might be better to have a range for its
-- returned descendants. If this or ancestor changes, need to change show2 and read2 as well. 
data GraphDescendant = IndexDesc ASIndex 

-- The output of a graph ancestor can be an index, pointer, or range, because that's what are in 
-- user-defined expressions
type GraphAncestor   = ASReference

descendantsToIndices :: [GraphDescendant] -> [ASIndex]
descendantsToIndices = map dToI
  where
    dToI (IndexDesc i) = i 

indicesToAncestryRequestInput :: [ASIndex] -> [AncestryRequestInput]
indicesToAncestryRequestInput = map IndexInput


----------------------------------------------------------------------------------------------------------------------------------------------
-- Delimiters
----------------------------------------------------------------------------------------------------------------------------------------------

data ExportData = ExportData { exportCells           :: [ASCell]
                             , exportBars            :: [Bar]
                             , exportDescriptors     :: [RangeDescriptor]
                             , exportCondFormatRules :: [CondFormatRule]
                             , exportHeaders         :: [EvalHeader] } deriving (Show, Read, Eq, Generic)

-- #incomplete Assumes the export has at least one cell. 
exportDataSheetId :: ExportData -> ASSheetId
exportDataSheetId = (view (cellLocation.locSheetId)) . $head . exportCells


----------------------------------------------------------------------------------------------------------------------------------------------
-- Delimiters

msgPartDelimiter = "`" -- TODO: should require real parsing instead of weird char strings
relationDelimiter = "&"
keyPartDelimiter :: String
keyPartDelimiter = "?"

-- TODO: should require real parsing instead of never-used unicode chars at some point
refDelimiter = '/'
keyTypeSeparator = "~"

-- TODO: hide this on export
splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy delimiter = foldr f [[]]
  where f c l@(x:xs) | c == delimiter = []:l
                     | otherwise = (c:x):xs

----------------------------------------------------------------------------------------------------------------------------------------------
-- instances

deriveSafeCopy 1 'base ''ExportData

-- compressed show

class Show2 a where
  show2 :: a -> String

class Read2 a where
  read2 :: (Show2 a) => String -> a

instance Show2 Col where
  show2 col = show $ col^.int

instance Show2 Row where
  show2 row = show $ row^.int

instance Show2 Coord where
  show2 coord = "(" ++ show (coord^.col^.int) ++ "," ++ show (coord^.row^.int) ++ ")"

-- Unfortunately doesn't have any type safety checks. on the order the values are passed in
-- Unfortunately the name here is bad.
-- extendedCoordCase :: ExtendedCoord -> a -> a -> a -> a
-- extendedCoordCase extendedCoord (valIfFiniteCoord, valIfInfiniteRows, valIfInfiniteCols) = undefined
-- rangeCase :: ASRange -> a -> a -> a -> a

instance Show2 ExtendedCoord where
  show2 extendedCoord =
    case (isFinite (extendedCoord^.extendedCol),
          isFinite (extendedCoord^.extendedRow)) of
      -- Coord (Finite 12) (Finite 14) -> (12, 14)
      (True, True) -> show2 $ fromExtendedCoord extendedCoord
      -- Coord (Finite 12) (Infinite) -> 12. In this case, the Coord is the br of a colrange.
      (True, False) -> show $ (fromInfinite $ extendedCoord^.extendedCol)^.int
      _ -> ""

-- Cases on which elements of extendedCoord are finite.
-- Currently only hase cases for finite ranges and colRanges.

instance Show2 ExtendedRect where
  show2 (coord1, extendedCoord2) = "(" ++ show2 coord1 ++ "," ++ show2 extendedCoord2 ++ ")" 

instance Show2 ASIndex where 
  show2 (Index sid a) = 'I':refDelimiter:(T.unpack sid) ++ (refDelimiter:(show2 a))

instance Show2 ASPointer where
  show2 (Pointer (Index sid a)) = 'P':refDelimiter:(T.unpack sid) ++ (refDelimiter:(show2 a))

-- Cases on whether a range is a colRange, or a Finite range.
-- Code duplication with exendedCoord.
instance Show2 ASRange where 
  show2 range@(Range sid a@(_, extendedCoord))
    | isColRange range = 'C':refDelimiter:(T.unpack sid) ++ (refDelimiter:(show2 a))
    | isFiniteRange range = 'R':refDelimiter:(T.unpack sid) ++ (refDelimiter:(show2 a))
    | otherwise = "DIDN'T GET A COLRANGE OR A FINITE RANGE"

instance (Show2 AncestryRequestInput) where
  show2 (IndexInput i) = show2 i
  show2 (RangeInput r) = show2 r

-- Requires that a <= extendedB, c <= extendedD
intervalIntersect :: (Ord a) => (a, Infinite a) -> (a, Infinite a) -> Bool
intervalIntersect (a, extendedB) (c, extendedD) = 
  inInterval (a, extendedB) c || inInterval (c, extendedD) a

instance Show2 GraphDescendant where
  show2 (IndexDesc i) = show2 i

instance Show2 ASReference where
  show2 (IndexRef il) = show2 il 
  show2 (RangeRef rl) = show2 rl
  show2 (PointerRef p) = show2 p
  show2 (OutOfBounds) = "OUTOFBOUNDS"

instance Show2 Dimensions where
  show2 dims = show ((width dims)^.int, (height dims)^.int)

instance Read2 ASReference where
  read2 str = loc
    where
      loc = case str of 
        "OUTOFBOUNDS" -> OutOfBounds
        _ -> loc' 
          where 
            (tag, sid, locstr) = case splitBy refDelimiter str of 
              [tag', sid', locstr'] -> (tag', sid', locstr')
              _ -> $error ("read2 :: ASReference failed to split string " ++ str)
            loc' = case tag of 
              "I" -> IndexRef $ Index (T.pack sid) (read2 locstr :: Coord)
              "P" -> PointerRef $ Pointer (Index (T.pack sid) (read2 locstr :: Coord))
              "R" -> RangeRef $ Range (T.pack sid) (read2 locstr :: ExtendedRect)
              "C" -> RangeRef $ Range (T.pack sid) (read2 locstr :: ExtendedRect)

-- Right now, pairToCoord is the identity map, but it is kept since future
-- changes to the type of Coord will make this function relevant.
pairToCoord :: (Int, Int) -> Coord
pairToCoord (a, b) = makeCoord (Col a) (Row b)

instance Read2 Coord where
  read2 str = pairToCoord ( $read str :: (Int, Int))
-- No read2 on Infinite a. All casing is done within reading ExtendedCoord.

-- Parsers for Read2 on ExtendedCoord. These are here temporarily until
-- I fix the parsing in the Graph.cpp. The final version of our code will not 
-- have these.


-- Read2 "((11, 12),(13,14))" = (11,12) (Finite 13, Finite 14)
-- Read2 "((11, 12),13)" = (11, 12), (Finite 13, Infinite)

instance Read2 ExtendedRect where
  read2 str = 
    case (readMaybe str :: Maybe ((Int, Int), Int)) of
      Just ((a, b), c) -> (makeCoord (Col a) (Row b), makeExtendedCoord (Finite $ Col c) Infinite)
      Nothing ->
        case (readMaybe str :: Maybe ((Int, Int), (Int, Int))) of
          Just ((a, b), (c, d)) -> (makeCoord (Col a) (Row b), makeExtendedCoord (Finite $ Col c) (Finite $ Row d))

instance Read2 ASIndex where 
  read2 str = case ((read2 :: String -> ASReference) str) of 
    IndexRef i -> i

instance Read2 ASRange where 
  read2 str = case ((read2 :: String -> ASReference) str) of 
    RangeRef r -> r

instance Read2 ASPointer where 
  read2 str = case ((read2 :: String -> ASReference) str) of 
    PointerRef p -> p

instance Read2 GraphDescendant where
  read2 s = IndexDesc (read2 s :: ASIndex)

instance Read2 Dimensions where
  read2 str = Dimensions { width = Col w, height = Row h }
    where (w, h) = $read str :: (Int, Int)

