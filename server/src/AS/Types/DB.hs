{-# LANGUAGE DeriveGeneric #-}

module AS.Types.DB
  ( module AS.Types.DB
  , module AS.Types.Commits
  ) where

import GHC.Generics

import AS.Types.Cell
import AS.Types.Commits
import AS.Types.Locations
import AS.Types.Eval
import AS.Types.CellProps

import Debug.Trace

import Prelude
import qualified Data.Text as T 
import qualified Data.List as L
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString               as B
import Data.Serialize (Serialize)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Transactions

data ASTransaction = Transaction {transactionCommitSource :: CommitSource,
                                  afterCells :: [ASCell],
                                  fatCells :: [RangeDescriptor],
                                  deletedLocations :: [ASIndex] }

----------------------------------------------------------------------------------------------------------------------------------------------
-- Graph queries

-- A relation (toLoc,[fromLoc]); a toLoc must be an index, a fromLoc can be any ancestor
type ASRelation = (ASIndex,[GraphAncestor])

-- Graph read (getX) and write (setX) requests
data GraphReadRequest = GetDescendants | GetImmediateDescendants | GetProperDescendants | GetImmediateAncestors deriving (Show)

-- Exporting/Importing
data ExportData = ExportData { exportCells :: [ASCell], exportDescriptors :: [RangeDescriptor] } deriving (Show, Read, Eq, Generic)


data GraphWriteRequest = SetRelations | Recompute | Clear deriving (Show)

-- Graph input for functions like getDescendants can be indexes or ranges. Getting the descendants 
-- of a range = descendants of decomposed indices in ranges
data GraphReadInput  = IndexInput ASIndex | RangeInput ASRange

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

indicesToGraphReadInput :: [ASIndex] -> [GraphReadInput]
indicesToGraphReadInput = map IndexInput



----------------------------------------------------------------------------------------------------------------------------------------------
-- Delimiters

msgPartDelimiter = "`" -- TODO: should require real parsing instead of weird char strings
relationDelimiter = "&"
keyPartDelimiter = '?'

-- TODO: should require real parsing instead of never-used unicode chars at some point
cellDelimiter = '©'
exprDelimiter = '®'
refDelimiter = '/'

-- TODO: hide this on export
splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy delimiter = foldr f [[]]
  where f c l@(x:xs) | c == delimiter = []:l
                     | otherwise = (c:x):xs

----------------------------------------------------------------------------------------------------------------------------------------------
-- instances

instance Serialize ExportData

-- compressed show

class Show2 a where
  show2 :: a -> String

class Read2 a where
  read2 :: (Show2 a) => String -> a

instance (Show2 ASCell) where
  show2 (Cell l e v ts) = (show2 l) ++ (cellDelimiter:(show2 e)) 
                          ++ (cellDelimiter:(show2 v)) ++ (cellDelimiter:(show ts))
instance (Show2 ASIndex) where 
  show2 (Index sid a) = 'I':refDelimiter:(T.unpack sid) ++ (refDelimiter:(show a))
instance (Show2 ASPointer) where
  show2 (Pointer sid a) = 'P':refDelimiter:(T.unpack sid) ++ (refDelimiter:(show a))
instance (Show2 ASRange) where 
  show2 (Range sid a) = 'R':refDelimiter:(T.unpack sid) ++ (refDelimiter:(show a))
instance (Show2 GraphReadInput) where
  show2 (IndexInput i) = show2 i
  show2 (RangeInput r) = show2 r
instance (Show2 GraphDescendant) where
  show2 (IndexDesc i) = show2 i
instance (Show2 ASReference) where
  show2 (IndexRef il) = show2 il 
  show2 (RangeRef rl) = show2 rl
  show2 (PointerRef p) = show2 p
  show2 (OutOfBounds) = "OUTOFBOUNDS"
instance (Show2 ASExpression) where
  show2 (Expression xp lang) = 'E':exprDelimiter:xp ++ (exprDelimiter:(show lang))
  show2 (Coupled xp lang dtype rangekey) = 'C':exprDelimiter:xp 
                                        ++ (exprDelimiter:(show lang)) 
                                        ++ (exprDelimiter:(show dtype)) 
                                        ++ (exprDelimiter:(show2 rangekey))

instance (Show2 ASValue) where
  show2 = show -- TODO optimize

instance (Show2 RangeKey) where
  show2 (RangeKey idx dims) = (show2 idx) 
                           ++ (keyPartDelimiter:(show dims)) 
                           ++ (keyPartDelimiter:"RANGEKEY")

-- compressed read

instance (Read2 ASCell) where
  read2 str = Cell l xp v ts
    where
      (l, xp, v, ts) = case splitBy cellDelimiter str of 
        [locstr, xpstr, valstr, tagstr] -> (read2 locstr :: ASIndex, read2 xpstr :: ASExpression, 
                                            read2 valstr :: ASValue, read tagstr :: ASCellProps)
        _ -> error ("read2 :: ASCell failed on string " ++ str)
instance (Read2 ASReference) where
  read2 str = loc
    where
      loc = case str of 
        "OUTOFBOUNDS" -> OutOfBounds
        _ -> loc' 
          where 
            (tag, sid, locstr) = case splitBy refDelimiter str of 
              [tag', sid', locstr'] -> (tag', sid', locstr')
              _ -> error ("read2 :: ASReference failed to split string " ++ str)
            loc' = case tag of 
              "I" -> IndexRef $ Index (T.pack sid) (read locstr :: Coord)
              "P" -> PointerRef $ Pointer (T.pack sid) (read locstr :: Coord)
              "R" -> RangeRef $ Range (T.pack sid) (read locstr :: (Coord, Coord))
instance (Read2 ASIndex) where 
  read2 str = case ((read2 :: String -> ASReference) str) of 
    IndexRef i -> i
instance (Read2 ASRange) where 
  read2 str = case ((read2 :: String -> ASReference) str) of 
    RangeRef r -> r
instance (Read2 ASPointer) where 
  read2 str = case ((read2 :: String -> ASReference) str) of 
    PointerRef p -> p
instance (Read2 GraphDescendant) where
  read2 s = IndexDesc (read2 s :: ASIndex)
instance (Read2 ASExpression)
  where
    read2 str = xp
      where
        splits = splitBy exprDelimiter str
        xp = case (head splits) of 
          "E" -> case (tail splits) of 
            [xp, lang] -> Expression xp (read lang :: ASLanguage)
            _ -> error $ "read2 splits expression incorrectly: " ++ str 
          "C" -> case (tail splits) of 
            [xp, lang, dtype, rangekey] -> Coupled xp (read lang :: ASLanguage) (read dtype :: ExpandingType) (read2 rangekey :: RangeKey)
            _ -> error $ "read2 splits expression incorrectly: " ++ str 
instance (Read2 ASValue)
  where 
    read2 = read -- TODO optimize

readCells :: String -> [ASCell]
readCells str = map (\c -> read2 c :: ASCell) $ splitBy ',' str

instance (Read2 RangeKey) where
  read2 str = RangeKey idx dims
    where
     idxStr:dimsStr:_ = splitBy keyPartDelimiter str
     idx = read2 idxStr :: ASIndex
     dims = read dimsStr :: Dimensions

----------------------------------------------------------------------------------------------------------------------
-- Redis key constructors

-- key for set of all fat cells in a sheet
makeSheetRangesKey :: ASSheetId -> B.ByteString
makeSheetRangesKey sid = BC.pack $ (T.unpack sid) ++ (keyPartDelimiter:"ALL_RANGES")

-- key for locations
makeLocationKey :: ASIndex -> B.ByteString
makeLocationKey = BC.pack . show2

-- key for sheet
makeSheetKey :: ASSheetId -> B.ByteString -- for storing the actual sheet as key-value
makeSheetKey = BC.pack . T.unpack

-- key for all location keys in a sheet
makeSheetSetKey :: ASSheetId -> B.ByteString
makeSheetSetKey sid = BC.pack $! (T.unpack sid) ++ "Locations"

-- key for workbook
makeWorkbookKey :: String -> B.ByteString
makeWorkbookKey = BC.pack