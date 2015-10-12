
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module AS.Types.Excel where

import AS.Types.Core
import Prelude
import GHC.Generics
import Data.List

import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Control.Monad.Except
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Excel Location Parsing

-- TODO fix recursion
data ExLoc = ExSheet {name :: String, sheetLoc :: ExLoc} |
             ExRange {first :: ExLoc, second :: ExLoc}     |
             ExIndex {d1 :: String, col :: String, d2 :: String, row :: String} 
             deriving (Show,Read,Eq,Ord)

showExcelLoc :: ExLoc -> String
showExcelLoc exLoc = case exLoc of
  ExSheet sheet rest -> sheet ++ "!" ++ (showExcelLoc rest)
  ExRange first second -> (showExcelLoc first) ++ ":" ++ (showExcelLoc second)
  ExIndex dol1 c dol2 r -> dol1 ++ c ++ dol2 ++ r

showExcelValue :: ASValue -> String
showExcelValue val = case val of
  ValueS s      -> show s
  ValueI i      -> show i
  ValueD d      -> show d
  ValueB b      -> show b 
  ValueL l      -> toExcelList $ fmap showExcelValue l

toExcelList :: [String] -> String
toExcelList lst  = "[" ++ (intercalate "," lst) ++ "]"


----------------------------------------------------------------------------------------------------------------------------------------------
-- | Excel core types

data ERef = ERef ASReference deriving (Show, Read, Eq, Ord)

data ENumeric = EValueI Int | EValueD Double deriving (Show,Read,Eq,Ord)

instance Num ENumeric where
  negate (EValueI i) = EValueI (-i)
  negate (EValueD d) = EValueD (-d)
  abs (EValueD d) = EValueD (abs d)
  abs (EValueI i) = EValueI (abs i)
  (+) (EValueD d) (EValueD d') = EValueD (d+d')
  (+) (EValueI i) (EValueD d) = EValueD ((fromIntegral i)+d)
  (+) (EValueD d) (EValueI i) = EValueD ((fromIntegral i)+d)
  (+) (EValueI i) (EValueI i') = EValueI (i+i')
  (*) (EValueD d) (EValueD d') = EValueD (d*d')
  (*) (EValueI i) (EValueD d) = EValueD ((fromIntegral i)*d)
  (*) (EValueD d) (EValueI i) = EValueD ((fromIntegral i)*d)
  (*) (EValueI i) (EValueI i') = EValueI (i*i')
  fromInteger a = (EValueD (fromIntegral a))

instance Fractional ENumeric where
  (/) (EValueD d) (EValueD d') = EValueD $ d/d'
  (/) (EValueI i) (EValueD d) = EValueD $ (fromIntegral i)/d
  (/) (EValueD d) (EValueI i) = EValueD $ (fromIntegral i)/d
  (/) (EValueI i) (EValueI i') = EValueD $ (fromIntegral i)/(fromIntegral i')

data EValue = 
  EBlank |
  EValueNum ENumeric |
  EValueB Bool |
  EValueS String |
  EValueE String
  deriving (Show, Read,Eq,Ord)

type Col = Int
type Row = Int
data EMatrix = EMatrix {emRows :: !Int, emCols :: !Int, content :: !(V.Vector EValue)} 
  deriving (Show, Read,Eq)
data EEntity = 
  EntityRef ERef | 
  EntityVal EValue | 
  EntityMatrix EMatrix  deriving (Show, Read,Eq)

--------------------------------------------------------------------------------------------------------------------------------------------
-- | Excel evaluation types

data Context = Context {evalMap :: M.Map ASIndex ASValue, curLoc :: ASReference}

type ThrowsError = Either EError
type EResult = ThrowsError EEntity

type EFunc =  (Context -> [EEntity] -> EResult)
type EFuncResult = (Context -> [EResult] -> EResult)

--------------------------------------------------------------------------------------------------------------
-- | Type checking for argument extraction

-- | Function name, argument number, arguments -> possibly return type
type ExtractArg a = (String -> Int -> [EEntity] -> ThrowsError a)

class EType a where
  extractType :: (EEntity -> Maybe a)
  getRequired :: String -> ExtractArg a
  getRequired typeName f i entities
    | length entities < i = Left $ RequiredArgMissing f i
    | otherwise = case (extractType entity) of
      Nothing -> Left $ ArgType f i typeName (getType entity)
      Just x  -> Right x
      where
        entity = entities!!(i-1)
  getOptional :: String -> a -> ExtractArg a
  getOptional typeName defaultVal f i entities 
    | length entities < i = Right defaultVal
    -- | Must be the correct type if it exists as an argument
    | otherwise = getRequired typeName f i entities
  getOptionalMaybe :: String -> ExtractArg (Maybe a)
  getOptionalMaybe typeName f i entities 
    | length entities < i = Right Nothing
    | otherwise = do 
        entity <- getRequired typeName f i entities
        return $ Just entity

instance EType Bool where
  extractType (EntityVal (EValueB b)) = Just b
  extractType _ = Nothing 

instance EType EValue where
  extractType (EntityVal v) = Just v
  extractType _ = Nothing

instance EType String where
  extractType (EntityVal (EValueS s)) = Just s
  extractType _ = Nothing

instance EType ERef where
  extractType (EntityRef r) = Just r
  extractType _ = Nothing

instance EType Int where
  extractType (EntityVal (EValueNum (EValueD d))) = Just $ floor d
  extractType (EntityVal (EValueNum (EValueI i))) = Just i
  extractType _ = Nothing

instance EType Double where
  extractType (EntityVal (EValueNum (EValueD d))) = Just $ d
  extractType (EntityVal (EValueNum (EValueI i))) = Just $ (fromIntegral i)
  extractType _ = Nothing

instance EType EMatrix where
  extractType (EntityMatrix m) = Just m
  extractType _ = Nothing

instance EType ENumeric where
  extractType (EntityVal (EValueNum n)) = Just n
  extractType _ = Nothing


getType :: EEntity -> String
getType (EntityRef _) = "ref"
getType (EntityVal (EValueS _)) = "string"
getType (EntityVal (EValueNum (EValueI _))) = "int"
getType (EntityVal (EValueNum _)) = "numeric"
getType (EntityVal (EValueB _)) = "bool"
getType (EntityMatrix m) = "matrix"
getType (EntityVal v) = "value"


-----------------------------------------------------------------------------
-- * Abstract syntax

-- | The type of formulas.
data BasicFormula = 
   Var EValue                      -- ^ Variables
 | Fun String [Formula]            -- ^ Fun
 | Ref ExLoc                    -- ^ Reference
 deriving (Show, Read)

data Formula = ArrayConst [[BasicFormula]] | Basic BasicFormula deriving (Show, Read)

type ContextualFormula = (Formula, Bool) -- designates arrayFormula or not

--data ExcelLoc = BareLoc ExcelBareLoc | SheetLoc ExcelSheetLoc | WorkbookLoc ExcelWorkbookLoc 
--data ExcelBareLoc = IndexLoc ExcelIndexLoc | RangeLoc ExcelRangeLoc 
--data ExcelIndexLoc = ExIndex {fixedRow :: Bool, fixedCol :: Bool, col :: String, row :: Int} 
--data ExcelRangeLoc = ExRange {topLeftLoc :: ExcelIndexLoc, bottomRightLoc :: ExcelIndexLoc} 
--data ExcelSheetLoc = ExSheet String ExcelBareLoc 
--data ExcelWorkbookLoc = ExWorkbook String ExcelSheetLoc
