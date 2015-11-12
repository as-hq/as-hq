module AS.Types.DB where

import AS.Types.Core
import AS.Util (splitBy)

import Prelude
import GHC.Generics
import Data.Aeson hiding (Success)
import Data.Text hiding (foldr, map)


data ASTransaction = Transaction {transactionCommitSource :: CommitSource,
                                  roots :: [ASCell], 
                                  afterCells :: [ASCell],
                                  lists :: [ASList]}

----------------------------------------------------------------------------------------------------------------------------------------------
-- Queries

data GraphQuery = 
  GetDescendants |
  GetImmediateAncestors |
  SetRelations | 
  RollbackGraph |
  Recompute |
  Clear
  deriving (Show)


----------------------------------------------------------------------------------------------------------------------------------------------
-- Delimiters

-- TODO: should require real parsing instead of never-used unicode chars at some point
cellDelimiter = '©'
exprDelimiter = '®'
refDelimiter = '/'


----------------------------------------------------------------------------------------------------------------------------------------------
-- Compressed read/show 

class Show2 a where
  show2 :: a -> String

instance Show2 ASCell where
  show2 (Cell l e v ts) = (show2 l) ++ (cellDelimiter:(show2 e)) 
                          ++ (cellDelimiter:(show2 v)) ++ (cellDelimiter:(show ts))

instance (Show2 ASIndex) where 
  show2 (Index sid a) = 'I':refDelimiter:(unpack sid) ++ (refDelimiter:(show a))

instance (Show2 ASRange) where 
  show2 (Range sid a) = 'R':refDelimiter:(unpack sid) ++ (refDelimiter:(show a))

instance (Show2 ASReference) where
  show2 (IndexRef il) = show2 il 
  show2 (RangeRef rl) = show2 rl
  show2 (OutOfBounds) = "OUTOFBOUNDS"


instance (Show2 ASExpression) where
  show2 (Expression xp lang) = 'E':exprDelimiter:xp ++ (exprDelimiter:(show lang))

instance (Show2 ASValue) where
  show2 = show -- TODO optimize


class Read2 a where
  read2 :: (Show2 a) => String -> a

instance (Read2 ASCell) where
  read2 str = Cell l xp v ts
    where
      (l, xp, v, ts) = case splitBy cellDelimiter str of 
        [locstr, xpstr, valstr, tagstr] -> (read2 locstr :: ASIndex, read2 xpstr :: ASExpression, 
                                            read2 valstr :: ASValue, read tagstr :: [ASCellTag])
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
              "I" -> IndexRef $ Index (pack sid) (read locstr :: Coord)
              "R" -> RangeRef $ Range (pack sid) (read locstr :: (Coord, Coord))

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
        [tag, midstr, laststr] = splitBy exprDelimiter str
        xp = case tag of 
          "E" -> Expression midstr (read laststr :: ASLanguage)

instance (Read2 ASValue)
  where 
    read2 = read -- TODO optimize

readCells :: String -> [ASCell]
readCells str = map (\c -> read2 c :: ASCell) $ splitBy ',' str
