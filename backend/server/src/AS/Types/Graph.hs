
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

instance Show2 Coord where
  show2 coord = "(" ++ show (coord^.col) ++ "," ++ show (coord^.row) ++ ")"

instance Show2 InfiniteRowCoord where
  show2 infiniteRowCoord = show $ infiniteRowCoord^.col

instance Show2 Rect where
  show2 (coord1, coord2) = "(" ++ show2 coord1 ++ "," ++ show2 coord2 ++ ")" 

instance Show2 (Coord, InfiniteRowCoord) where
  show2 (coord1, infRowCoord2) = "(" ++ show2 coord1 ++ "," ++ show2 infRowCoord2 ++ ")" 

instance (Show2 ASIndex) where 
  show2 (Index sid a) = 'I':refDelimiter:(T.unpack sid) ++ (refDelimiter:(show2 a))

instance (Show2 ASPointer) where
  show2 (Pointer (Index sid a)) = 'P':refDelimiter:(T.unpack sid) ++ (refDelimiter:(show2 a))

instance (Show2 ASRange) where 
  show2 (Range sid a) = 'R':refDelimiter:(T.unpack sid) ++ (refDelimiter:(show2 a))

instance (Show2 ASColRange) where 
  show2 (ColRange sid a) = 'C':refDelimiter:(T.unpack sid) ++ (refDelimiter:(show2 a))

instance (Show2 AncestryRequestInput) where
  show2 (IndexInput i) = show2 i
  show2 (RangeInput r) = show2 r

instance (Show2 GraphDescendant) where
  show2 (IndexDesc i) = show2 i

instance (Show2 ASReference) where
  show2 (IndexRef il) = show2 il 
  show2 (RangeRef rl) = show2 rl
  show2 (ColRangeRef cr) = show2 cr
  show2 (PointerRef p) = show2 p
  show2 (OutOfBounds) = "OUTOFBOUNDS"

instance (Show2 Dimensions) where
  show2 dims = show (width dims, height dims)

instance (Read2 ASReference) where
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
              "R" -> RangeRef $ Range (T.pack sid) (read2 locstr :: Rect)
              "C" -> ColRangeRef $ ColRange (T.pack sid) (read2 locstr :: (Coord, InfiniteRowCoord))

pairToCoord :: (Int, Int) -> Coord
pairToCoord (x, y) = Coord x y

instance (Read2 Coord) where
  read2 str = pairToCoord ($read str :: (Int, Int))

instance (Read2 InfiniteRowCoord) where
  read2 str = InfiniteRowCoord $ ($read str)

instance (Read2 Rect) where
  read2 str =  (pairToCoord pair1, pairToCoord pair2)
    where
      (pair1, pair2) = $read str :: ((Int, Int), (Int, Int))

instance (Read2 (Coord, InfiniteRowCoord)) where
  read2 str = (pairToCoord pair, InfiniteRowCoord col)
    where
      (pair, col) = $read str :: ((Int, Int), Int)

instance (Read2 ASIndex) where 
  read2 str = case ((read2 :: String -> ASReference) str) of 
    IndexRef i -> i

instance (Read2 ASRange) where 
  read2 str = case ((read2 :: String -> ASReference) str) of 
    RangeRef r -> r

instance (Read2 ASColRange) where 
  read2 str = case ((read2 :: String -> ASReference) str) of 
    ColRangeRef r -> r

instance (Read2 ASPointer) where 
  read2 str = case ((read2 :: String -> ASReference) str) of 
    PointerRef p -> p

instance (Read2 GraphDescendant) where
  read2 s = IndexDesc (read2 s :: ASIndex)

instance (Read2 Dimensions) where
  read2 str = Dimensions { width = w, height = h }
    where (w, h) = $read str :: (Int, Int)

