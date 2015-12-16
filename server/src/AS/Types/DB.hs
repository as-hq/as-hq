{-# LANGUAGE OverloadedStrings, DeriveGeneric, DataKinds, KindSignatures, GADTs #-}

module AS.Types.DB
  ( module AS.Types.DB
  , module AS.Types.Commits
  ) where

import Prelude
import GHC.Generics

import AS.Types.Cell
import AS.Types.Commits
import AS.Types.Locations
import AS.Types.Eval
import AS.Types.CellProps
import AS.Types.RowColProps

import Debug.Trace

import Data.List.Split (splitOn)
import qualified Data.Text as T 
import qualified Data.List as L
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString               as B
import qualified Data.Serialize as S

----------------------------------------------------------------------------------------------------------------------------------------------
-- Graph queries

-- A relation (toLoc,[fromLoc]); a toLoc must be an index, a fromLoc can be any ancestor
type ASRelation = (ASIndex, [GraphAncestor])

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

instance S.Serialize ExportData

-- compressed show

class Show2 a where
  show2 :: a -> String

class Read2 a where
  read2 :: (Show2 a) => String -> a

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

instance (Read2 Dimensions) where
  read2 str = Dimensions { width = w, height = h }
    where (w, h) = read str :: (Int, Int)

----------------------------------------------------------------------------------------------------------------------
-- Redis keys 

data RedisKeyType = 
    SheetRangesType 
  | SheetType 
  | WorkbookType 
  | EvalHeaderType 
  | TempCommitType
  | PushCommitType 
  | PopCommitType 
  | LastMessageType 
  | CFRulesType 
  | RCPropsType 
  | AllWorkbooksType 
  | AllSheetsType
  | VolatileLocsType
  | RangeType
  deriving (Show, Read)

data RedisKey :: RedisKeyType -> * where
  SheetRangesKey  :: ASSheetId -> RedisKey SheetRangesType
  SheetKey        :: ASSheetId -> RedisKey SheetType
  WorkbookKey     :: WorkbookName -> RedisKey WorkbookType
  EvalHeaderKey   :: ASSheetId -> ASLanguage -> RedisKey EvalHeaderType
  TempCommitKey   :: CommitSource -> RedisKey TempCommitType
  PushCommitKey   :: CommitSource -> RedisKey PushCommitType
  PopCommitKey    :: CommitSource -> RedisKey PopCommitType
  LastMessageKey  :: CommitSource -> RedisKey LastMessageType
  CFRulesKey      :: ASSheetId -> RedisKey CFRulesType
  RCPropsKey      :: ASSheetId -> RowColType -> Int -> RedisKey RCPropsType
  AllWorkbooksKey :: RedisKey AllWorkbooksType 
  AllSheetsKey    :: RedisKey AllSheetsType
  VolatileLocsKey :: RedisKey VolatileLocsType
  RedisRangeKey   :: RangeKey -> RedisKey RangeType

instance Show2 (RedisKey a) where
  show2 k = case k of 
    SheetRangesKey sid      -> (keyPrefix SheetRangesType) ++ T.unpack sid
    SheetKey sid            -> (keyPrefix SheetType) ++ T.unpack sid
    WorkbookKey wname       -> (keyPrefix WorkbookType) ++ wname
    EvalHeaderKey sid lang  -> (keyPrefix EvalHeaderType) ++ (T.unpack sid) ++ keyPartDelimiter ++ (show lang)
    TempCommitKey c         -> (keyPrefix TempCommitType) ++ show c
    PushCommitKey c         -> (keyPrefix PushCommitType) ++ show c
    PopCommitKey c          -> (keyPrefix PopCommitType) ++ show c
    LastMessageKey c        -> (keyPrefix LastMessageType) ++ show c
    CFRulesKey sid          -> (keyPrefix CFRulesType) ++ T.unpack sid
    RCPropsKey sid rct ind  -> (keyPrefix RCPropsType) ++ (T.unpack sid) ++ keyPartDelimiter ++ (show rct) ++ keyPartDelimiter ++ (show ind) -- #refactor this show
    AllWorkbooksKey         -> keyPrefix AllWorkbooksType
    AllSheetsKey            -> keyPrefix AllSheetsType
    VolatileLocsKey         -> keyPrefix VolatileLocsType
    RedisRangeKey (RangeKey idx dims) -> (keyPrefix RangeType) ++ (show2 idx) ++ keyPartDelimiter ++ (show2 dims)

-- value-dependent instances mothafucka!
instance Read2 (RedisKey RangeType) where
  read2 s = RedisRangeKey rkey
    where 
      [typeStr, keyStr] = splitOn keyTypeSeparator s
      [idxStr, dimsStr] = splitOn keyPartDelimiter keyStr
      rkey = case (read typeStr :: RedisKeyType) of 
        RangeType -> RangeKey (read2 idxStr :: ASIndex) (read2 dimsStr :: Dimensions)

instance Read2 (RedisKey RCPropsType) where
    read2 s = RCPropsKey sid rct ind
      where
        [typeStr, keyStr] = splitOn keyTypeSeparator s
        [sidStr, rctStr, indStr] = splitOn keyPartDelimiter keyStr
        sid = T.pack sidStr
        rct = read rctStr :: RowColType
        ind = read indStr :: Int

instance Show CommitSource where
  show (CommitSource sid uid) = (T.unpack sid) ++ keyPartDelimiter ++ (T.unpack uid)

keyPrefix :: RedisKeyType -> String
keyPrefix kt = (show kt) ++ keyTypeSeparator

keyPattern :: RedisKeyType -> String
keyPattern kt = (keyPrefix kt) ++ "*" 

keyPatternBySheet :: RedisKeyType -> ASSheetId -> String
keyPatternBySheet kt sid = 
  let sid' = T.unpack sid 
  in (keyPrefix kt) ++ case kt of 
    TempCommitType  -> sid' ++ "*"
    PushCommitType  -> sid' ++ "*"
    PopCommitType   -> sid' ++ "*"
    LastMessageType -> sid' ++ "*"

rcPropsKeyPattern :: ASSheetId -> RowColType -> String
rcPropsKeyPattern sid rct = (keyPrefix RCPropsType) ++ (T.unpack sid) ++ keyPartDelimiter ++ (show rct) ++ "*"

toRedisFormat :: RedisKey a -> B.ByteString
toRedisFormat = BC.pack . show2