{-# LANGUAGE TypeFamilies, DeriveGeneric, TemplateHaskell #-}

module AS.Types.CondFormat where

import AS.ASJSON 

import AS.Types.Cell
import AS.Types.Locations
import AS.Types.CellProps
import AS.Types.Errors
import Data.List
import AS.Types.Updates

import GHC.Generics
import Data.Aeson.Types
import Data.SafeCopy
import qualified Data.Text as T

type CondFormatRuleId = T.Text

data CondFormatRule = CondFormatRule { condFormatRuleId :: CondFormatRuleId
                                     , cellLocs :: [ASRange]
                                     , formatMapConstructor :: FormatMapConstructor }  
                                       deriving (Show, Read, Generic, Eq)
                                       -- condFormatCondition :: CondFormatCondition }

type LambdaConditionExpr = String

data FormatMapConstructor = BoolFormatMapConstructor { boolFormatMapCondition :: BoolCondition
                                                     , boolFormatMapProp :: CellProp }
                          | LambdaFormatMapConstructor LambdaConditionExpr
                          deriving (Show, Read, Generic, Eq)

data BoolCondition = 
    CustomBoolCond ASExpression
  | NoExprBoolCond NoExprBoolCondType
  | OneExprBoolCond OneExprBoolCondType ASExpression  
  | TwoExprBoolCond TwoExprBoolCondType ASExpression ASExpression
  deriving (Show, Read, Generic, Eq)

data NoExprBoolCondType = IsEmpty | IsNotEmpty deriving (Show, Read, Generic, Eq)
data OneExprBoolCondType = GreaterThan | LessThan | Geq | Leq | Equals | NotEquals deriving (Show, Read, Generic, Eq)
data TwoExprBoolCondType = IsBetween | IsNotBetween deriving (Show, Read, Generic, Eq)

symbolTableLookup0 :: NoExprBoolCondType -> (ASValue -> Bool)
symbolTableLookup0 IsEmpty    = (== NoValue)
symbolTableLookup0 IsNotEmpty = (/= NoValue)

symbolTableLookup1 :: OneExprBoolCondType -> (ASValue -> ASValue -> Bool)
symbolTableLookup1 GreaterThan = (>)
symbolTableLookup1 LessThan    = (<)
symbolTableLookup1 Geq         = (>=)
symbolTableLookup1 Leq         = (<=)
symbolTableLookup1 Equals      = (==)
symbolTableLookup1 NotEquals   = (/=)

symbolTableLookup2 :: TwoExprBoolCondType -> (ASValue -> ASValue -> ASValue -> Bool)
symbolTableLookup2 IsBetween    = isBetween
symbolTableLookup2 IsNotBetween = \x y z -> not $ isBetween x y z

checkBoolCond ::  BoolCondition -> ASValue -> (ASExpression -> EitherTExec ASValue) -> EitherTExec Bool
checkBoolCond (CustomBoolCond xp) _ evalExpr = (ValueB True ==) <$> evalExpr xp
checkBoolCond (NoExprBoolCond typ) v _ = return . symbolTableLookup0 typ $ v
checkBoolCond (OneExprBoolCond typ xp) v1 evalExpr = do 
  v2 <- evalExpr xp 
  return $ symbolTableLookup1 typ v1 v2
checkBoolCond (TwoExprBoolCond typ xp1 xp2) v1 evalExpr = do 
  [v2, v3] <- mapM evalExpr [xp1, xp2]
  return $ symbolTableLookup2 typ v1 v2 v3

-- | Defines the ordering used in GreaterThanCondition, IsBetweenCondition, ...
-- This ordering is only used in CondFormat thus far. Hence it is in this file.
-- TODO: timchu, 12/17/15. this Ord is not exactly right. Should be the same as
-- the ordering on Evalues. This has not been vetted for completeness or tested
-- for correctness.
-- This is temporary. Defining these ineqs should be done in one place in the codebase
-- and implemented for both ASValues and EValues.
instance Ord ASValue where
  -- TODO: is this right?
  (<=) NoValue v        = (<=) (ValueI 0) v
  (<=) v NoValue        = (<=) (ValueI 0) v
  (<=) (ValueB True)  v = (<=) (ValueI 1) v
  (<=) v (ValueB True)  = (<=) v (ValueI 1)
  (<=) (ValueB False) v = (<=) (ValueI 0) v
  (<=) v (ValueB False) = (<=) v (ValueI 0)

  (<=) (ValueS s) (ValueI i) = False
  (<=) (ValueI i) (ValueS s) = False

  (<=) (ValueS s) (ValueD d) = False
  (<=) (ValueD d) (ValueS s) = False

  (<=) (ValueI i) (ValueD d) = (<=) (fromIntegral i) d
  (<=) (ValueD d) (ValueI i) = (<=) d (fromIntegral i)

  (<=) (ValueS s1) (ValueS s2) = (<=) s1 s2
  (<=) (ValueI i1) (ValueI i2) = (<=) i1 i2
  (<=) (ValueD d1) (ValueD d2) = (<=) d1 d2
  (<=) x1 x2 = False

-- tests if an ASValue is between a1 and a2 inclusive. Uses the Ord defined on ASValue above.
isBetween :: ASValue -> ASValue -> ASValue -> Bool
isBetween value a1 a2 = value >= min a1 a2 && max a1 a2 >= value

type CondFormatRuleDiff = Diff CondFormatRule
type CondFormatRuleUpdate = Update CondFormatRule CondFormatRuleId

instance HasKey CondFormatRule where
  type KeyType CondFormatRule = CondFormatRuleId
  key = condFormatRuleId

asToFromJSON ''CondFormatRule
asToFromJSON ''CondFormatRuleDiff
asToFromJSON ''CondFormatRuleUpdate
asToFromJSON ''FormatMapConstructor
asToFromJSON ''NoExprBoolCondType
asToFromJSON ''OneExprBoolCondType
asToFromJSON ''TwoExprBoolCondType

deriveSafeCopy 1 'base ''CondFormatRule
deriveSafeCopy 1 'base ''FormatMapConstructor
deriveSafeCopy 1 'base ''BoolCondition
deriveSafeCopy 1 'base ''NoExprBoolCondType
deriveSafeCopy 1 'base ''OneExprBoolCondType
deriveSafeCopy 1 'base ''TwoExprBoolCondType

-- This type exists *solely* to make conditional formatting rules easier to convert to 
-- JSON's in a format that's friendly to the frontend. 
data BoolCondition' =
  CustomCondition ASExpression
  | IsEmptyCondition
  | IsNotEmptyCondition
  | GreaterThanCondition ASExpression
  | LessThanCondition ASExpression
  | GeqCondition ASExpression
  | LeqCondition ASExpression
  | EqualsCondition ASExpression
  | NotEqualsCondition ASExpression
  | IsBetweenCondition ASExpression ASExpression
  | IsNotBetweenCondition ASExpression ASExpression
  deriving (Show, Read, Generic, Eq)

boolConditionToBoolCondition' :: BoolCondition -> BoolCondition'
boolConditionToBoolCondition' (CustomBoolCond xp) = CustomCondition xp
boolConditionToBoolCondition' (NoExprBoolCond IsEmpty) = IsEmptyCondition
boolConditionToBoolCondition' (NoExprBoolCond IsNotEmpty) = IsNotEmptyCondition
boolConditionToBoolCondition' (OneExprBoolCond GreaterThan xp) = GreaterThanCondition xp
boolConditionToBoolCondition' (OneExprBoolCond LessThan xp) = LessThanCondition xp
boolConditionToBoolCondition' (OneExprBoolCond Geq xp) = GeqCondition xp
boolConditionToBoolCondition' (OneExprBoolCond Leq xp) = LeqCondition xp
boolConditionToBoolCondition' (OneExprBoolCond Equals xp) = EqualsCondition xp
boolConditionToBoolCondition' (OneExprBoolCond NotEquals xp) = NotEqualsCondition xp
boolConditionToBoolCondition' (TwoExprBoolCond IsBetween xp1 xp2) = IsBetweenCondition xp1 xp2
boolConditionToBoolCondition' (TwoExprBoolCond IsNotBetween xp1 xp2) = IsNotBetweenCondition xp1 xp2

boolCondition'ToBoolCondition :: BoolCondition' -> BoolCondition
boolCondition'ToBoolCondition (CustomCondition xp) = CustomBoolCond xp
boolCondition'ToBoolCondition (IsEmptyCondition) = NoExprBoolCond IsEmpty
boolCondition'ToBoolCondition (IsNotEmptyCondition) = NoExprBoolCond IsNotEmpty
boolCondition'ToBoolCondition (GreaterThanCondition xp) = OneExprBoolCond GreaterThan xp
boolCondition'ToBoolCondition (LessThanCondition xp) = OneExprBoolCond LessThan xp
boolCondition'ToBoolCondition (GeqCondition xp) = OneExprBoolCond Geq xp
boolCondition'ToBoolCondition (LeqCondition xp) = OneExprBoolCond Leq xp
boolCondition'ToBoolCondition (EqualsCondition xp) = OneExprBoolCond Equals xp
boolCondition'ToBoolCondition (NotEqualsCondition xp) = OneExprBoolCond NotEquals xp
boolCondition'ToBoolCondition (IsBetweenCondition xp1 xp2) = TwoExprBoolCond IsBetween xp1 xp2
boolCondition'ToBoolCondition (IsNotBetweenCondition xp1 xp2) = TwoExprBoolCond IsNotBetween xp1 xp2


asToFromJSON ''BoolCondition'
instance ToJSON BoolCondition where
  toJSON = toJSON . boolConditionToBoolCondition'

instance FromJSON BoolCondition where 
  parseJSON obj = boolCondition'ToBoolCondition <$> (parseJSON obj :: Parser BoolCondition')