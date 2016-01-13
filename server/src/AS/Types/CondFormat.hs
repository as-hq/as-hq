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
                                     , condFormatCondition :: CondFormatCondition }  
                                       deriving (Show, Read, Generic, Eq)
                                       -- condFormatCondition :: CondFormatCondition }

type LambdaConditionExpr = String

data CondFormatCondition = BoolCondition BoolCondition CellProp | LambdaCondition LambdaConditionExpr
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
asToFromJSON ''CondFormatCondition
asToFromJSON ''NoExprBoolCondType
asToFromJSON ''OneExprBoolCondType
asToFromJSON ''TwoExprBoolCondType

deriveSafeCopy 1 'base ''CondFormatRule
deriveSafeCopy 1 'base ''CondFormatCondition
deriveSafeCopy 1 'base ''BoolCondition
deriveSafeCopy 1 'base ''NoExprBoolCondType
deriveSafeCopy 1 'base ''OneExprBoolCondType
deriveSafeCopy 1 'base ''TwoExprBoolCondType


instance ToJSON BoolCondition where
  toJSON boolCondition = case boolCondition of 
    CustomBoolCond xp           -> object [ "tag" .= ("CustomBoolCond" :: String), 
                                            "contents" .= xp ] 
    NoExprBoolCond typ          -> object [ "tag" .= typ ]
    OneExprBoolCond typ xp1     -> object [ "tag" .= typ, 
                                            "contents" .= xp1 ]
    TwoExprBoolCond typ xp1 xp2 -> object [ "tag" .= typ, 
                                            "contents" .= [xp1, xp2] ]

instance FromJSON BoolCondition
  parseJSON (Object v) = do
    contents <- v .: "contents" :: (Parser String)
    case length contents of 
      "evaluate" -> EvaluateReply <$> v .:? "value" <*> v .:? "error" <*> v .:? "display"
      "get_status" -> return GetStatusReply -- TODO
      "autocomplete" -> return AutocompleteReply -- TODO
      "clear" -> ClearReply <$> v .: "success"