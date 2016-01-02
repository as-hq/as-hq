-- TODO: split this up to, and separate types into core types and Excel types? 

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module AS.Types.Excel where

import AS.Types.CellProps
import AS.Types.Locations
import AS.Types.Eval
import AS.Types.Errors
import AS.Types.Sheets

import Prelude
import GHC.Generics
import Data.List

import Text.Read

import qualified Data.Vector     as V
import qualified Data.Char       as C
import qualified Data.Map.Strict as M
import Control.Monad.Except
import Data.Maybe
import Control.Monad (liftM, ap)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Text as T
import Database.Redis (Connection)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Excel Location Parsing

-- reference locking
data SingleRefType = ABS | REL deriving (Eq)
data RefType = ABS_ABS | ABS_REL | REL_ABS | REL_REL deriving (Eq)

data ExLoc   = ExIndex {refType :: RefType, col :: String, row :: String} 
  deriving (Eq)
data ExCol = ExCol { singleRefType :: SingleRefType, col2 :: String}
  deriving (Eq)
data ExColRange = ExColRange {firstCoord :: ExLoc, secondCol :: ExCol}
  deriving (Eq)
data ExRange = ExRange {first :: ExLoc, second :: ExLoc}
   deriving (Eq)
data ExRef   = 
    ExLocRef {exLoc :: ExLoc, locSheet :: Maybe SheetName, locWorkbook :: Maybe WorkbookName}
  | ExColRangeRef {exColRange :: ExColRange, colRangeSheet :: Maybe SheetName, colRangeWorkbook :: Maybe WorkbookName}
  | ExRangeRef {exRange :: ExRange, rangeSheet :: Maybe SheetName, rangeWorkbook :: Maybe WorkbookName}
  | ExPointerRef {pointerLoc :: ExLoc, pointerSheet :: Maybe SheetName, pointerWorkbook :: Maybe WorkbookName}
  | ExOutOfBounds 
  deriving (Eq)

-- convenience class so all refs can use "sheetRef" etc.
class Ref a where
  sheetRef :: a -> Maybe String
  workbookRef :: a -> Maybe String

instance Ref ExRef where
  sheetRef a = case a of 
    ExLocRef _ _ _ -> locSheet a
    ExRangeRef _ _ _ -> rangeSheet a
    ExColRangeRef _ _ _ -> colRangeSheet a
    ExPointerRef _ _ _ -> pointerSheet a
    ExOutOfBounds -> Nothing
  workbookRef a = case a of 
    ExLocRef _ _ _ -> locWorkbook a
    ExRangeRef _ _ _ -> rangeWorkbook a
    ExColRangeRef _ _ _ -> colRangeWorkbook a
    ExPointerRef _ _ _ -> pointerWorkbook a
    ExOutOfBounds -> Nothing

instance Show ExRef where
  show a = 
    let prefix = showRefQualifier (workbookRef a) (sheetRef a)
    in case a of 
      ExOutOfBounds                -> "#REF!"
      ExLocRef l _ _               -> prefix ++ (show l)
      ExColRangeRef (ExColRange tl r) _ _ -> prefix ++ (show tl) ++ ":" ++ (show r)
      ExRangeRef (ExRange tl br) _ _ -> prefix ++ (show tl) ++ ":" ++ (show br)
      ExPointerRef l _ _           -> '@':prefix ++ (show l)

instance Show ExCol where
  show (ExCol t c) = d1 ++ c
    where
      d1 = case t of
        ABS -> "$"
        REL -> ""

instance Show ExLoc where
  show (ExIndex rType c r) = d1 ++ c ++ d2 ++ r
    where 
      (d1, d2) = case rType of 
        ABS_ABS -> ("$","$")
        ABS_REL -> ("$","")
        REL_ABS -> ("","$")
        REL_REL -> ("","")

showRefQualifier :: Maybe WorkbookName -> Maybe SheetName -> String
showRefQualifier wb sh = case wb of 
  Just wb' -> case sh of 
    Just sh' -> wb' ++ "!" ++ sh' ++ "!"
    Nothing  -> ""
  Nothing  -> case sh of 
    Just sh' -> sh' ++ "!"
    Nothing  -> ""

showExcelValue :: ASValue -> String
showExcelValue val = case val of
  ValueS s      -> show s
  ValueI i      -> show i
  ValueD d      -> show d
  ValueB b      -> show b

toExcelList :: [String] -> String
toExcelList lst  = "[" ++ (intercalate "," lst) ++ "]"

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Excel core types

data ERef = ERef ASReference deriving (Show, Read, Eq, Ord)

data ENumeric = EValueI Integer | EValueD Double deriving (Show)


instance Eq ENumeric where
  (==) (EValueD d) (EValueD d') = d==d'
  (==) (EValueI i) (EValueD d)  = (fromIntegral i)==d
  (==) (EValueD d) (EValueI i)  = (fromIntegral i)==d
  (==) (EValueI i) (EValueI i') = i==i'

instance Ord ENumeric where
  (<=) (EValueI i) (EValueI i') = i <= i'
  (<=) (EValueI i) (EValueD d) = (fromIntegral i) <= d
  (<=) (EValueD d) (EValueI i') = d <= (fromIntegral i')
  (<=) (EValueD d) (EValueD d') = d <= d'

instance Num ENumeric where
  negate (EValueI i) = EValueI (-i)
  negate (EValueD d) = EValueD (-d)
  signum (EValueI i) = EValueI $ signum i
  signum (EValueD d) = EValueD $ signum d
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
  fromInteger a = (EValueI (fromIntegral a))

intExp :: ENumeric -> ENumeric -> ENumeric
intExp (EValueI b) (EValueI e)
  | e >= 0    = EValueI $ b ^ e
  | otherwise = EValueD $ (fromIntegral b) ^^ e
intExp (EValueD b) (EValueI e) = EValueD $ b ^^ e

floatExp :: ENumeric -> ENumeric -> ENumeric
floatExp (EValueI b) (EValueD e) = EValueD $ (fromIntegral b) ** e
floatExp (EValueD b) (EValueD e) = EValueD $ b ** e

instance Fractional ENumeric where
  (/) (EValueD d) (EValueD d') = EValueD $ d/d'
  (/) (EValueI i) (EValueD d) = EValueD $ (fromIntegral i)/d
  (/) (EValueD d) (EValueI i) = EValueD $ d/(fromIntegral i)
  (/) (EValueI i) (EValueI i') = EValueD $ (fromIntegral i)/(fromIntegral i')
  fromRational r = EValueD $ fromRational r

type EFormattedNumeric = Formatted ENumeric

instance Ord EFormattedNumeric where
  (<=) (Formatted x _) (Formatted y _) = x <= y

instance (Show a) => Show (Formatted a) where 
  show (Formatted x _) = show x

instance Num EFormattedNumeric where
  negate = liftM negate
  signum (Formatted f _) = Formatted (signum f) $ Just NoFormat
  abs = liftM abs
  (+) = liftM2 (+)
  (*) (Formatted x (Just Percentage)) (Formatted y f) = Formatted (x*y) f
  (*) (Formatted x f) (Formatted y (Just Percentage)) = Formatted (x*y) f
  (*) x y = liftM2 (*) x y
  fromInteger = return . fromInteger

instance Fractional EFormattedNumeric where
  (/) = liftM2 (/)
  fromRational = return . fromRational

data EValue =
  EBlank | -- value doesn't exist in DB
  EMissing | -- missing argument
  EValueNum EFormattedNumeric |
  EValueB Bool |
  EValueS String |
  EValueE String
  deriving (Show, Eq)

-- #needsrefactor really shouldn't make this an instance of Ord, since we should really allow
-- more graceful error handling. 
instance Ord EValue where
  -- TODO: is this right?
  (<=) (EBlank) v = (<=) (EValueNum $ return (EValueI 0)) v
  (<=) v (EBlank) = (<=) v (EValueNum $ return (EValueI 0))
  (<=) (EValueB True) v = (<=) (EValueNum $ return (EValueI 1)) v
  (<=) (EValueB False) v = (<=) (EValueNum $ return (EValueI 0)) v
  (<=) v (EValueB True) = (<=) v (EValueNum $ return (EValueI 1))
  (<=) v (EValueB False) = (<=) v (EValueNum $ return (EValueI 0))
  (<=) (EValueNum (Formatted n1 _)) (EValueNum (Formatted n2 _)) = (<=) n1 n2
  (<=) (EValueS s1) (EValueS s2) = (<=) s1 s2
  (<=) (EValueS s) (EValueNum n) = (<=) (strToEValueNum s) (EValueNum n)
  (<=) (EValueNum n) (EValueS s) = (<=) (EValueNum n) (strToEValueNum s)
  (<=) _ _ = error "Invalid comparison" 

strToEValueNum :: String -> EValue
strToEValueNum str = case (readMaybe str :: Maybe Double) of 
  Just d -> EValueNum $ return $ EValueD d
  Nothing -> error "Failed to convert string to number"

data EMatrix = EMatrix {emCols :: !Int, emRows :: !Int, content :: !(V.Vector EValue)}
  deriving (Show, Eq)
data EEntity =
  EntityRef ERef |
  EntityVal EValue |
  EntityMatrix EMatrix  deriving (Show, Eq)

--------------------------------------------------------------------------------------------------------------------------------------------
-- | Excel evaluation types

data Context = Context {evalMap :: CellMap, curLoc :: ASIndex, dbConn :: Connection}

type ThrowsError = Either EError
type EResult = ThrowsError EEntity

type EFunc =  (Context -> [EEntity] -> EResult)
type EFuncResult = (Context -> [EResult] -> EResult)

--------------------------------------------------------------------------------------------------------------
-- | Type checking for argument extraction

-- | Function name, argument number, arguments -> possibly return type
type ExtractArg a = (String -> Int -> [EEntity] -> ThrowsError a)

class EType a where
  -- | Can the entity be cast into type a? Implemented by all instances.
  extractType :: (EEntity -> Maybe a)

    -- getRequired is getRequired' with the type passed in as the first argument.
    -- It gets implemented for each instance of EType.
  getRequired :: ExtractArg a
    -- | If the argument exists and is of the right type, return that type. Else return an error.
  getRequired' :: String -> ExtractArg a
  getRequired' typeName f i entities
    | length entities < i = Left $ RequiredArgMissing f i
    | otherwise = case (extractType entity) of
        Nothing -> Left $ ArgType f i typeName (getType entity)
        Just x  -> Right x
        where
          entity = entities!!(i-1)
  -- | Same as above, but allow for a default value (optional argument)
  getOptional :: a -> ExtractArg a
  getOptional' :: String -> a -> ExtractArg a
  getOptional' typeName defaultVal f i entities
    | length entities < i = Right defaultVal
    -- | If the value is missing, return default
    -- | Must be the correct type if it exists as an argument
    | otherwise = case (entities!!(i-1)) of
      EntityVal EMissing -> Right defaultVal
      otherwise -> getRequired' typeName f i entities
  -- | Same as above, but no default value (just return Nothing if the argument doesn't exist)
  getOptionalMaybe :: ExtractArg (Maybe a)
  getOptionalMaybe' :: String -> ExtractArg (Maybe a)
  getOptionalMaybe' typeName f i entities
    | length entities < i = Right Nothing
    | otherwise = do
        entity <- getRequired' typeName f i entities
        return $ Just entity

-- NOTE: treating index refs as 1x1 matrices if they're replaced, but some functions still want numerics, for example, not 1x1 matrices
  -- The first line of most functions below addresses this
  
instance EType Bool where
  extractType (EntityMatrix (EMatrix 1 1 v)) = extractType $ EntityVal $ V.head v
  extractType (EntityVal (EValueB b)) = Just b
  extractType _ = Nothing
  getRequired = getRequired' "bool"
  getOptional = getOptional' "bool"
  getOptionalMaybe = getOptionalMaybe' "bool"

instance EType EValue where
  extractType (EntityMatrix (EMatrix 1 1 v)) = extractType $ EntityVal $ V.head v
  extractType (EntityVal v) = Just v
  extractType _ = Nothing
  getRequired = getRequired' "value"
  getOptional = getOptional' "value"
  getOptionalMaybe = getOptionalMaybe' "value"

instance EType String where
  extractType (EntityMatrix (EMatrix 1 1 v)) = extractType $ EntityVal $ V.head v
  extractType (EntityVal (EValueS s)) = Just s
  extractType _ = Nothing
  getRequired = getRequired' "string"
  getOptional = getOptional' "string"
  getOptionalMaybe = getOptionalMaybe' "string"

instance EType ERef where
  extractType (EntityRef r) = Just r
  extractType _ = Nothing

instance EType Integer where
  extractType (EntityMatrix (EMatrix 1 1 v)) = extractType $ EntityVal $ V.head v
  extractType (EntityVal (EValueNum (Formatted (EValueD d) _))) = Just $ floor d
  extractType (EntityVal (EValueNum (Formatted (EValueI i) _))) = Just i
  extractType _ = Nothing
  getRequired = getRequired' "int"
  getOptional = getOptional' "int"
  getOptionalMaybe = getOptionalMaybe' "int"

instance EType Int where
  extractType (EntityMatrix (EMatrix 1 1 v)) = extractType $ EntityVal $ V.head v
  extractType (EntityVal (EValueNum (Formatted (EValueD d) _))) = Just $ floor d
  extractType (EntityVal (EValueNum (Formatted (EValueI i) _))) = Just $ fromIntegral i
  extractType _ = Nothing
  getRequired = getRequired' "int"
  getOptional = getOptional' "int"
  getOptionalMaybe = getOptionalMaybe' "int"

instance EType Double where
  extractType (EntityMatrix (EMatrix 1 1 v)) = extractType $ EntityVal $ V.head v
  extractType (EntityVal (EValueNum (Formatted (EValueD d) _))) = Just d
  extractType (EntityVal (EValueNum (Formatted (EValueI i) _))) = Just $ fromIntegral i
  extractType _ = Nothing
  getRequired = getRequired' "double"
  getOptional = getOptional' "double"
  getOptionalMaybe = getOptionalMaybe' "double"

instance EType EMatrix where
  extractType (EntityMatrix m) = Just m
  extractType _ = Nothing
  getRequired = getRequired' "matrix"
  getOptional = getOptional' "matrix"
  getOptionalMaybe = getOptionalMaybe' "matrix"


instance EType EFormattedNumeric where
  extractType (EntityMatrix (EMatrix 1 1 v)) = extractType $ EntityVal $ V.head v
  extractType (EntityVal (EValueNum n)) = Just n
  extractType _ = Nothing
  getRequired = getRequired' "numeric"
  getOptional = getOptional' "numeric"
  getOptionalMaybe = getOptionalMaybe' "numeric"

-- | Print the type, useful for error messages
getType :: EEntity -> String
getType (EntityRef _) = "ref"
getType (EntityVal (EValueS _)) = "string"
getType (EntityVal (EValueNum (Formatted (EValueI _) _))) = "int"
getType (EntityVal (EValueNum _)) = "double"
getType (EntityVal (EValueB _)) = "bool"
getType (EntityVal (EValueE _)) = "err"
getType (EntityMatrix m) = "matrix"
getType (EntityVal v) = "value"

-----------------------------------------------------------------------------
-- Abstract syntax

-- | The type of formulas.
data BasicFormula =
   Var EValue                      -- Variables
 | Fun String [Formula]            -- Function
 | Ref ExRef                       -- Reference
 deriving (Show)

data Formula = ArrayConst [[BasicFormula]] | Basic BasicFormula deriving (Show)
-- designates arrayFormula or not
data ContextualFormula = ArrayFormula Formula | SimpleFormula Formula deriving (Show)

-----------------------------------------------------------------------------
-- Conversions between ASRef's and ExRef's

-- | "AA" -> 27
colStrToInt :: String -> Int
colStrToInt "" = 0
colStrToInt (c:cs) = 26^(length(cs)) * coef + colStrToInt cs
  where
    coef = fromJust (elemIndex (C.toUpper c) ['A'..'Z']) + 1

-- | 27 -> "AA",  218332954 ->"RITESH"
intToColStr :: Int -> String
intToColStr x
  | x <= 26 = [['A'..'Z'] !! (x-1)]
  | otherwise = intToColStr d ++ [['A'..'Z'] !! m]
      where
        m = (x-1) `mod` 26
        d = (x-1) `div` 26

-- used in DB Ranges
indexToExcel :: ASIndex -> String
indexToExcel (Index _ (c,r)) = (intToColStr c) ++ (show r)

-- | Turns an Excel reference to an AlphaSheets reference. (first arg is the sheet of the
-- ref, unless it's a part of the ExRef)
exRefToASRef :: ASSheetId -> ExRef -> ASReference
exRefToASRef sid exRef = case exRef of
  ExOutOfBounds -> OutOfBounds
  ExLocRef (ExIndex _ c r) sn wn -> IndexRef $ Index sid' (colStrToInt c, read r :: Int)
    where sid' = maybe sid id (sheetIdFromContext sn wn)
  ExColRangeRef (ExColRange f (ExCol _ c2)) sn wn -> ColRangeRef $ ColRange sid' (tl, colStrToInt c2)
    where
      sid' = maybe sid id (sheetIdFromContext sn wn)
      IndexRef (Index  _ tl) = exRefToASRef sid' $ ExLocRef f sn Nothing
  ExRangeRef (ExRange f s) sn wn -> RangeRef $ Range sid' (tl, br)
    where
      sid' = maybe sid id (sheetIdFromContext sn wn)
      IndexRef (Index _ tl) = exRefToASRef sid' $ ExLocRef f sn Nothing
      IndexRef (Index _ br) = exRefToASRef sid' $ ExLocRef s sn Nothing
  ExPointerRef (ExIndex _ c r) sn wn -> PointerRef $ Pointer sid' (colStrToInt c, read r :: Int)
    where sid' = maybe sid id (sheetIdFromContext sn wn)

-- #incomplete we should actually be looking in the db. For now, with the current UX of
-- equating sheet names and sheet id's with the dialog box, 
sheetIdToSheetName :: ASSheetId -> Maybe SheetName
sheetIdToSheetName = Just . T.unpack

-- #incomplete lol. just returns sheet name from sheet id for now. 
sheetIdFromContext :: Maybe SheetName -> Maybe WorkbookName -> Maybe ASSheetId
sheetIdFromContext (Just sn) _ = Just $ T.pack sn
sheetIdFromContext _ _ = Nothing
