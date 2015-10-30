{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module AS.Types.Excel where

import AS.Types.Core
import Prelude
import GHC.Generics
import Data.List

import Text.Read

import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Control.Monad.Except
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Excel Location Parsing

-- d1 = $ or nothing; $ means absolute column, nothing means relative. ditto for d2 but for rows
data ExLoc   = ExIndex {d1 :: String, col :: String, d2 :: String, row :: String} | ExOutOfBounds deriving (Show, Read, Eq, Ord)
data ExRange = ExRange {first :: ExLoc, second :: ExLoc} deriving (Show, Read, Eq, Ord)
data ExLocOrRange = ExLoc1 ExLoc | ExRange1 ExRange deriving (Show, Read, Eq, Ord)
data ExRef = ExLocOrRangeRef ExLocOrRange | ExSheetLocOrRangeRef String ExLocOrRange  deriving (Show, Read, Eq, Ord)
-- I think this is the simplest grammar we can write that actually correctly captures the type we want.
-- It's quite ugly as it is though -- I imagine it can be refactored with lenses / better names, but this
-- seems not very urgent as of now. (10/9)
--
-- Also doesn't have any support for columns, workbooks, or 3D reference. (10/9)

showExcelRef :: ExRef -> String
showExcelRef exRef = case exRef of
  ExSheetLocOrRangeRef sheet rest -> sheet ++ "!" ++ (showExcelRef (ExLocOrRangeRef rest))
  ExLocOrRangeRef (ExRange1 (ExRange first second)) -> (showExcelRef $ ExLocOrRangeRef $ ExLoc1 $ first) ++ ":" ++ (showExcelRef $ ExLocOrRangeRef $ ExLoc1 second)
  ExLocOrRangeRef (ExLoc1 (ExIndex dol1 c dol2 r)) -> dol1 ++ c ++ dol2 ++ r
  ExLocOrRangeRef (ExLoc1 ExOutOfBounds) -> "#REF!"


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

data ENumeric = EValueI Int | EValueD Double deriving (Show,Read)

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
  fromInteger a = (EValueD (fromIntegral a))

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
  (/) (EValueD d) (EValueI i) = EValueD $ (fromIntegral i)/d
  (/) (EValueI i) (EValueI i') = EValueD $ (fromIntegral i)/(fromIntegral i')
  fromRational r = EValueD $ fromRational r

data EValue =
  EBlank | -- value doesn't exist in DB
  EMissing | -- missing argument
  EValueNum ENumeric |
  EValueB Bool |
  EValueS String |
  EValueE String
  deriving (Show, Read,Eq)

-- #needsrefactor really shouldn't make this an instance of Ord, since we should really allow
-- more graceful error handling. 
instance Ord EValue where
  -- TODO: is this right?
  (<=) (EBlank) v = (<=) (EValueNum (EValueI 0)) v
  (<=) v (EBlank) = (<=) v (EValueNum (EValueI 0))
  (<=) (EValueB True) v = (<=) (EValueNum (EValueI 1)) v
  (<=) (EValueB False) v = (<=) (EValueNum (EValueI 0)) v
  (<=) v (EValueB True) = (<=) v (EValueNum (EValueI 1))
  (<=) v (EValueB False) = (<=) v (EValueNum (EValueI 0))
  (<=) (EValueNum n1) (EValueNum n2) = (<=) n1 n2
  (<=) (EValueS s1) (EValueS s2) = (<=) s1 s2
  (<=) (EValueS s) (EValueNum n) = (<=) (strToEValueNum s) (EValueNum n)
  (<=) (EValueNum n) (EValueS s) = (<=) (EValueNum n) (strToEValueNum s)
  (<=) _ _ = error "Invalid comparison"

strToEValueNum :: String -> EValue
strToEValueNum str = case (readMaybe str :: Maybe Double) of 
  Just d -> EValueNum $ EValueD d
  Nothing -> error "Failed to convert string to number"

type Col = Int
type Row = Int
data EMatrix = EMatrix {emCols :: !Int, emRows :: !Int, content :: !(V.Vector EValue)}
  deriving (Show, Read,Eq)
data EEntity =
  EntityRef ERef |
  EntityVal EValue |
  EntityMatrix EMatrix  deriving (Show, Read,Eq)

--------------------------------------------------------------------------------------------------------------------------------------------
-- | Excel evaluation types

data Context = Context {evalMap :: M.Map ASReference ASValue, curLoc :: ASReference}

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
  -- | If the argument exists and is of the right type, return that type. Else return an error.
  getRequired :: String -> ExtractArg a
  getRequired typeName f i entities
    | length entities < i = Left $ RequiredArgMissing f i
    | otherwise = case (extractType entity) of
        Nothing -> Left $ ArgType f i typeName (getType entity)
        Just x  -> Right x
        where
          entity = entities!!(i-1)
  -- | Same as above, but allow for a default value (optional argument)
  getOptional :: String -> a -> ExtractArg a
  getOptional typeName defaultVal f i entities
    | length entities < i = Right defaultVal
    -- | If the value is missing, return default
    -- | Must be the correct type if it exists as an argument
    | otherwise = case (entities!!(i-1)) of
      (EntityVal EMissing) -> Right defaultVal
      otherwise -> getRequired typeName f i entities
  -- | Same as above, but no default value (just return Nothing if the argument doesn't exist)
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

-- | Print the type, useful for error messages
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
   Var EValue                      -- Variables
 | Fun String [Formula]            -- Function
 | Ref ExRef                       -- Reference
 deriving (Show, Read)

data Formula = ArrayConst [[BasicFormula]] | Basic BasicFormula deriving (Show, Read)
-- designates arrayFormula or not
data ContextualFormula = ArrayFormula Formula | SimpleFormula Formula deriving (Show, Read)

