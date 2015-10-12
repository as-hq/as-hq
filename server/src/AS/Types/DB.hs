module AS.Types.DB where

import AS.Types.Core
import AS.Util (splitBy)

import Prelude
import GHC.Generics
import Data.Aeson hiding (Success)
import Data.Text hiding (foldr, map)


data ASTransaction = Transaction {transactionUserId :: ASUserId,
                                  transactionSheetId :: ASSheetId,
                                  roots :: [ASCell], 
                                  beforeCells :: [ASCell], 
                                  afterCells :: [ASCell],
                                  lists :: [ASList]}

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Queries

data GraphQuery = 
  GetDescendants |
  GetImmediateAncestors |
  SetRelations 
  deriving (Show)


----------------------------------------------------------------------------------------------------------------------------------------------
-- | compressed read/show 

class Show2 a where
  show2 :: a -> String

instance Show2 ASCell where
  show2 (Cell l e v ts) = (show2 l) ++ ('|':(show2 e)) ++ ('|':(show2 v)) ++ ('|':(show ts))

instance (Show2 ASIndex) where 
  show2 (Index sid a) = "I/" ++ (unpack sid) ++ ('/':(show a))

instance (Show2 ASRange) where 
  show2 (Range sid a) = "R/" ++ (unpack sid) ++ ('/':(show a))

instance (Show2 ASReference) where
  show2 (IndexRef il) = show2 il 
  show2 (RangeRef rl) = show2 rl

instance (Show2 ASExpression) where
  show2 (Expression xp lang) = "E?" ++ xp ++ ('?':(show lang))

instance (Show2 ASValue) where
  show2 = show -- TODO optimize


class Read2 a where
  read2 :: (Show2 a) => String -> a

instance (Read2 ASCell) where
  read2 str = Cell l xp v ts
    where
      [locstr, xpstr, valstr, tagstr] = splitBy '|' str
      l = read2 locstr :: ASIndex
      xp = read2 xpstr :: ASExpression
      v = read2 valstr :: ASValue
      ts = read tagstr :: [ASCellTag]

instance (Read2 ASReference) where
  read2 str = loc
    where
      [tag, sid, locstr] = splitBy '/' str
      loc = case tag of 
        "I" -> IndexRef $ Index (pack sid) (read locstr :: (Int, Int))
        "R" -> RangeRef $ Range (pack sid) (read locstr :: ((Int, Int), (Int, Int)))

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
        [tag, midstr, laststr] = splitBy '?' str
        xp = case tag of 
          "E" -> Expression midstr (read laststr :: ASLanguage)

instance (Read2 ASValue)
  where 
    read2 = read -- TODO optimize

readCells :: String -> [ASCell]
readCells str = map (\c -> read2 c :: ASCell) $ splitBy ',' str
