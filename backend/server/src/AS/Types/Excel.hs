-- TODO: split this up to, and separate types into core types and Excel types? 

{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module AS.Types.Excel where

import Prelude()
import AS.Prelude hiding (isInfinite)

import AS.Types.CellProps
import AS.Types.Errors
import AS.Types.Eval
import AS.Types.Formats
import AS.Types.Locations
import AS.Types.DataModification
import AS.Types.Infinites
import AS.Types.Sheets

import Data.List

import qualified Data.Vector     as V
import qualified Data.Char       as C
import qualified Data.Map.Strict as M
import Control.Monad.Except
import Control.Monad (liftM, (>=>))

import Text.Read (readMaybe)
import Data.Maybe hiding (fromJust)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Text as T
import Database.Redis (Connection)
import Control.Lens hiding ((.=), index, Context)
import Control.Lens.TH
import Control.Monad.Trans.Either

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Excel Location Parsing

-- Ord is necessary to compare ExItem a, which is needed to orient
-- colRanges.
data RefType = ABS | REL deriving (Eq, Show, Ord, Data, Typeable)

-- Type constructor of one parameter used to build ExCol and ExRow when Col and
-- Row are passed  in respectively.
data ExItem a = ExItem { _refType :: RefType, _ind :: a } deriving (Eq, Functor, Data, Typeable)
makeLenses ''ExItem

-- Ord is necessary to orient exRanges.
instance Ord a => Ord (ExItem a) where
  (>=) exItem1 exItem2 =
    exItem1^.ind >= exItem2^.ind ||
      exItem1^.ind == exItem2^.ind && exItem1^.refType >= exItem2^.refType
  (<=) exItem1 exItem2 = 
    exItem1^.ind <= exItem2^.ind ||
      exItem1^.ind == exItem2^.ind && exItem1^.refType <= exItem2^.refType

type ExCol = ExItem Col
type ExRow = ExItem Row
-- $A2 in Excel is turned into (ExItem ABS (Col1), ExItem REL (Row 2))
type ExIndex = (ExCol, ExRow)
type ExtExIndex = (Infinite ExCol, Infinite ExRow)

-- Creating lenses for ExIndex, ExtExIndex. Equivalent to the fst and snd lenses on pairs, except with types restricted.
exCol :: Lens' (ExCol, ExRow) ExCol
exCol = _1
exRow :: Lens' (ExCol, ExRow) ExRow
exRow = _2
extExCol :: Lens' (Infinite ExCol, Infinite ExRow) (Infinite ExCol)
extExCol = _1
extExRow :: Lens' (Infinite ExCol, Infinite ExRow) (Infinite ExRow)
extExRow = _2

data ExRange = ExRange {first :: ExIndex, second :: ExtExIndex}
   deriving (Eq, Data, Typeable)

data ExTemplateExpr = 
  ExSampleExpr {exSamples :: Int, exSampledIndex :: ExIndex} deriving (Eq, Data)

data ExRef   =
    ExIndexRef {exIndex :: ExIndex, locSheet :: Maybe SheetName, locWorkbook :: Maybe WorkbookName}
  | ExRangeRef {exRange :: ExRange, rangeSheet :: Maybe SheetName, rangeWorkbook :: Maybe WorkbookName}
  | ExPointerRef {pointerLoc :: ExIndex, pointerSheet :: Maybe SheetName, pointerWorkbook :: Maybe WorkbookName}
  | ExTemplateRef {exTemplate :: ExTemplateExpr, templateSheet :: Maybe SheetName, templateWorkbook :: Maybe WorkbookName}
  | ExOutOfBounds 
  deriving (Eq, Data, Typeable)

makeExIndex :: ExCol -> ExRow -> ExIndex
makeExIndex exCol exRow = (exCol, exRow)

-- Creates an exRange (oriented) from an ExIndex and an ExtExIndex.
-- Used in Parsing for ColRanges. This function should always be used
-- instead of the ExRange constructor since all exRanges should be oriented.
makeExRange :: ExIndex -> ExtExIndex -> ExRange
makeExRange eIndex1 eIndex2 =
  ExRange (l, t) (r, b)
    where 
    t = fromInfinite $ min row1 row2 -- This will never be an error.
    l = fromInfinite $ min col1 col2 -- This will never be an error.
    b = max row1 row2
    r = max col1 col2
    col1 = Finite $ eIndex1^.exCol
    col2 = eIndex2^.extExCol
    row1 = Finite $ eIndex1^.exRow
    row2 = eIndex2^.extExRow

-- Creates an exRange (oriented) from an ExIndex and an ExtExIndex.
-- Used only in Parsing for ColRanges.
makeExColRange :: ExIndex -> ExCol -> ExRange
makeExColRange eIndex eCol = makeExRange eIndex (Finite eCol, Infinite)

-- Creates an finite oriented exRange from an ExIndex and an ExIndex.
makeFiniteExRange :: ExIndex -> ExIndex -> ExRange
makeFiniteExRange eIndex1 (c2, r2) = makeExRange eIndex1 (Finite c2, Finite r2)

-- convenience class so all refs can use "sheetRef" etc.
class Ref a where
  sheetRef :: a -> Maybe String
  workbookRef :: a -> Maybe String

instance Ref ExRef where
  sheetRef a = case a of 
    ExIndexRef {} -> locSheet a
    ExRangeRef {} -> rangeSheet a
    ExPointerRef {} -> pointerSheet a
    ExTemplateRef {} -> templateSheet a
    ExOutOfBounds -> Nothing
  workbookRef a = case a of 
    ExIndexRef {} -> locWorkbook a
    ExRangeRef {} -> rangeWorkbook a
    ExPointerRef {} -> pointerWorkbook a
    ExTemplateRef {} -> templateWorkbook a
    ExOutOfBounds -> Nothing

instance Show ExRef where
  show a = 
    let prefix = showRefQualifier (workbookRef a) (sheetRef a)
    in case a of 
      ExOutOfBounds                -> "#REF!"
      ExIndexRef l _ _               -> prefix ++ (showExcel l)
      ExRangeRef (ExRange tl br) _ _ -> prefix ++ (showExcel tl) ++ ":" ++ (showExcel br)
      ExPointerRef l _ _           -> '@':prefix ++ (showExcel l)
      ExTemplateRef l _ _           -> prefix ++ showExcel l

displayRefType :: RefType -> String
displayRefType ABS = "$"
displayRefType REL = ""

showAsCol :: ExCol -> String
showAsCol exCol = (displayRefType $ exCol^.refType) ++ (colToColStr $ exCol^.ind)

showAsRow :: ExRow -> String
showAsRow exRow = (displayRefType $ exRow^.refType) ++ (rowToRowStr $ exRow^.ind)

-- Creating a showExcel instance for ExCol and ExRows and (ExCol, ExRow)s.
-- This is only used because show is already defined on pairs, and is only used
-- to create Show on ExRef. This should NOT be used outside Show on ExRef.
class ShowExcel a where
  showExcel :: a -> String

instance ShowExcel ExIndex where
  showExcel (c, r) = cStr ++ rStr 
    where cStr = showAsCol c
          rStr = showAsRow r

instance ShowExcel (Infinite ExCol, Infinite ExRow) where
  showExcel (Finite i, Infinite) = showAsCol i
  showExcel (Infinite, Finite j) = showAsRow j
  showExcel (Finite i, Finite j) = showAsCol i ++ showAsRow j
  -- This should never happen
  showExcel (Infinite, Infinite) = ""

instance ShowExcel ExTemplateExpr where
  showExcel (ExSampleExpr sampleNum ind) = "!{" ++ show sampleNum ++ ", " ++ showExcel ind ++ "}"


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
  signum (Formatted f _) = Formatted (signum f) (Just (Format NoFormat Nothing))
  abs = liftM abs
  (+) = liftM2 (+)
  (*) (Formatted x (Just (Format Percentage _))) (Formatted y f) = Formatted (x*y) f
  (*) (Formatted x f) (Formatted y (Just (Format Percentage _))) = Formatted (x*y) f
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
  (<=) _ _ = $error "Invalid comparison" 

strToEValueNum :: String -> EValue
strToEValueNum str = maybe err (EValueNum . return . EValueD) (readMaybe str :: Maybe Double)
  where err = $error "Failed to convert string to number"

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
type EFuncEitherT =  (Context -> [EEntity] -> EitherT EError IO EEntity)
type EFuncResultEitherT = (Context -> [EResult] -> EitherT EError IO EEntity)

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
    --  If the argument exists and is of the right type, return that type. Else return an error.
  getRequired' :: String -> ExtractArg a
  getRequired' typeName f i entities
    | length entities < i = Left $ RequiredArgMissing f i
    | otherwise = maybe (Left $ ArgType f i typeName $ getType entity) Right $ extractType entity
        where
          entity = entities!!(i-1)
  -- | Same as above, but allow for a default value (optional argument)
  getOptional :: a -> ExtractArg a
  getOptional' :: String -> a -> ExtractArg a
  getOptional' typeName defaultVal f i entities
    | length entities < i = Right defaultVal
    -- If the value is missing, return default
    -- Must be the correct type if it exists as an argument
    | otherwise = case (entities!!(i-1)) of
      EntityVal EMissing -> Right defaultVal
      otherwise -> getRequired' typeName f i entities
  -- Same as above, but no default value (just return Nothing if the argument doesn't exist)
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
  getRequired = getRequired' "referenece"
  getOptional = getOptional' "reference"
  getOptionalMaybe = getOptionalMaybe' "reference"

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

-- | "AA" -> Col 27
-- Used in colToColStr and in Excel/Lib.
intToColStr :: Int -> String
intToColStr x
  | x <= 26 = [['A'..'Z'] !! (x-1)]
  | otherwise = intToColStr d ++ [['A'..'Z'] !! m]
      where
        m = (x-1) `mod` 26
        d = (x-1) `div` 26

-- | "AA" -> Col 27
colToColStr :: Col -> String
colToColStr  = intToColStr . view int

-- | Col 27 -> "AA",  Col 218332954 ->"RITESH"
colStrToCol :: String -> Col
colStrToCol = Col . colStrToInt
  where 
    colStrToInt "" = 0
    colStrToInt (c:cs) = 26^(length(cs)) * coef + colStrToInt cs
      where
        coef = $fromJust (elemIndex (C.toUpper c) ['A'..'Z']) + 1

-- | Row 27 -> "27"
rowToRowStr :: Row -> String
rowToRowStr  = intToRowStr . view int
  where
    intToRowStr i = show i

-- | "27" -> Row 27
rowStrToRow :: String -> Row
rowStrToRow = Row . rowStrToInt
  where
    rowStrToInt r = $read r :: Int

-- used in DB Ranges
indexToExcel :: ASIndex -> String
indexToExcel (Index _ coord) = (colToColStr $ coord^.col) ++ (rowToRowStr $ coord^.row)

-- Turns an exIndex to an infiniteCol
exIndexToCoord :: ExIndex -> Coord
exIndexToCoord exIndex = makeCoord (exIndex^.exCol^.ind) (exIndex^.exRow^.ind)

-- Removes the Excel reference from a possibly infinite Excel Int.
-- Only used to convert exIndex to exCoord
removeRefType :: Infinite (ExItem a) -> Infinite a
removeRefType = fmap _ind

extExIndexToExtCoord :: ExtExIndex -> ExtendedCoord
extExIndexToExtCoord exIndex =
  makeExtendedCoord (removeRefType $ exIndex^.extExCol) (removeRefType $ exIndex^.extExRow)

-- | Turns an Excel reference to an AlphaSheets reference. (first arg is the sheet of the
-- ref, unless it's a part of the ExRef)
exRefToASRef :: ASSheetId -> ExRef -> ASReference
exRefToASRef sid exRef = case exRef of
  ExOutOfBounds -> OutOfBounds
  ExIndexRef exIndex sn wn -> IndexRef $ Index sid' $ exIndexToCoord exIndex
    where sid' = maybe sid id (sheetIdFromContext sn wn)
    -- VV probably needs orienting 
  ExRangeRef (ExRange f s) sn wn -> RangeRef $ Range sid' (tl, br)
    where
      sid' = maybe sid id (sheetIdFromContext sn wn)
      tl = exIndexToCoord f
      br = extExIndexToExtCoord s
  ExPointerRef exIndex sn wn -> PointerRef $ Pointer $ Index sid' $ exIndexToCoord exIndex
    where sid' = maybe sid id (sheetIdFromContext sn wn)
  ExTemplateRef t sn wn -> case t of 
    ExSampleExpr n coord -> TemplateRef $ SampleExpr n $ Index sid' $ exIndexToCoord coord
      where sid' = maybe sid id (sheetIdFromContext sn wn)

-- #incomplete we should actually be looking in the db. For now, with the current UX of
-- equating sheet names and sheet id's with the dialog box, 
sheetIdToSheetName :: ASSheetId -> Maybe SheetName
sheetIdToSheetName = Just . T.unpack

-- #incomplete lol. just returns sheet name from sheet id for now. 
sheetIdFromContext :: Maybe SheetName -> Maybe WorkbookName -> Maybe ASSheetId
sheetIdFromContext (Just sn) _ = Just $ T.pack sn
sheetIdFromContext _ _ = Nothing

-- outputs an exRange equivalent to the input of the first ExRange, with the first coord <= second coord
-- #lenses
-- TODO: Introduce PossiblyInfiniteRange as a type: is just correct, makes colRange functions  consequence of functions on ranges.
-- Note: Code duplication between this and orientRange.

------------------------------------------------------------------------------------------------------------------------------------------------

-- shifts both relative and absolute references.
-- checks in bounds. Shifts an ExCol by a Col, an ExRow by a Row.
-- F stands for "Forced".
shiftExItemF :: (Num a, Ord a) => a -> ExItem a -> Maybe (ExItem a)
shiftExItemF a exItem =
  -- ghc turns (newInd > 0) into (newInd  > fromInteger 0)
  -- where fromInteger is a function in the Num Typeclass.
  if newInd > 0
     then Just $ exItem & ind .~ newInd
     else Nothing
    where newInd = a + exItem^.ind

-- shifts only Absolute references.
-- NF stands for "Not Forced."
shiftExItemNF :: (Num a, Ord a) => a -> ExItem a -> Maybe (ExItem a)
shiftExItemNF a exItem =
  case exItem^.refType of
    ABS -> Just $ exItem
    REL -> shiftExItemF a exItem

-- shifts rows and cols in an exRef
-- if any row or col is out of bounds, output is ExOutOfBounds.
shiftExRefNF :: Offset -> ExRef -> ExRef
shiftExRefNF o =
  let shiftExRef = deepGMapM (shiftExItemNF $ dCol o) >=> deepGMapM (shiftExItemNF $ dRow o)
  in
  fromMaybe ExOutOfBounds . shiftExRef

-- shifts both relative and absolute references.
shiftExRefForced :: Offset -> ExRef -> ExRef
shiftExRefForced o =
  let shiftExRef = deepGMapM (shiftExItemF $ dCol o) >=> deepGMapM (shiftExItemF $ dRow o)
  in
  fromMaybe ExOutOfBounds . shiftExRef
