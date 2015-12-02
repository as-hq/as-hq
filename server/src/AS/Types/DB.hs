module AS.Types.DB
  ( module AS.Types.DB
  , module AS.Types.Commits
  ) where

import AS.Types.Cell
import AS.Types.Commits
import Debug.Trace

import Prelude
import qualified Data.Text as T 

----------------------------------------------------------------------------------------------------------------------------------------------
-- Transactions

data ASTransaction = Transaction {transactionCommitSource :: CommitSource,
                                  afterCells :: [ASCell],
                                  fatCells :: [RangeDescriptor],
                                  deletedLocations :: [ASIndex] }

----------------------------------------------------------------------------------------------------------------------------------------------
-- Queries

data GraphQuery = 
  GetDescendants | GetImmediateDescendants | GetProperDescendants |
  GetImmediateAncestors |
  SetRelations | 
  RollbackGraph |
  Recompute |
  Clear
  deriving (Show)


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
-- Compressed read/show 

-- Show

class Show2 a where
  show2 :: a -> String

instance Show2 ASCell where
  show2 (Cell l e v ts) = (show2 l) ++ (cellDelimiter:(show2 e)) 
                          ++ (cellDelimiter:(show2 v)) ++ (cellDelimiter:(show ts))

instance (Show2 ASIndex) where 
  show2 (Index sid a) = 'I':refDelimiter:(T.unpack sid) ++ (refDelimiter:(show a))
  show2 (Pointer sid a) = 'P':refDelimiter:(T.unpack sid) ++ (refDelimiter:(show a))

instance (Show2 ASRange) where 
  show2 (Range sid a) = 'R':refDelimiter:(T.unpack sid) ++ (refDelimiter:(show a))

instance (Show2 ASReference) where
  show2 (IndexRef il) = show2 il 
  show2 (RangeRef rl) = show2 rl
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

-- Read

class Read2 a where
  read2 :: (Show2 a) => String -> a

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
              "P" -> IndexRef $ Pointer (T.pack sid) (read locstr :: Coord)
              "R" -> RangeRef $ Range (T.pack sid) (read locstr :: (Coord, Coord))

instance (Read2 ASIndex) where 
  read2 str = case ((read2 :: String -> ASReference) str) of 
    IndexRef i -> i

instance (Read2 ASRange) where 
  read2 str = case ((read2 :: String -> ASReference) str) of 
    RangeRef r -> r

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