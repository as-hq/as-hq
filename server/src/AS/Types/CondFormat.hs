{-# LANGUAGE DeriveGeneric #-}

--TODO: timchu, can clean up these imports.
module AS.Types.CondFormat where

import AS.Types.DB (ASCommit)
import AS.Types.Cell
import AS.Types.Locations
import AS.Types.CellProps

import GHC.Generics
import Data.Aeson hiding (Success)
import Data.Aeson.Types (defaultOptions)
import Data.Serialize (Serialize)
import qualified Data.Text as T


instance ToJSON CondFormatRule
instance FromJSON CondFormatRule

instance Serialize CondFormatRule
instance Serialize CondFormatCondition
instance Serialize TwoExpressionsType
instance Serialize OneExpressionType
instance Serialize NoExpressionsType


data CondFormatRule = CondFormatRule { cellLocs :: [ASRange],
                                       condition :: CondFormatCondition,
                                       condFormat :: CellProp } deriving (Show, Read, Generic, Eq)

-- TODO: Timchu, 12/14/15. This is MVP; does not have date expressions or text expressions.
-- CustomExpressions are separate from OneExpresssionConditions since
-- OneExpressionConditions work by evaluating the expression in the condition, then applying the
-- relevant function (example: GreaterThan) to the value in the cell.
data CondFormatCondition =
    CustomExpressionCondition ASExpression
  | NoExpressionsCondition NoExpressionsType
  | OneExpressionCondition OneExpressionType ASExpression
  | TwoExpressionsCondition TwoExpressionsType ASExpression ASExpression
   deriving (Show, Read, Generic, Eq)


data OneExpressionType = GreaterThan | Equals | Geq | Leq | LessThan | NotEquals
  deriving (Show, Read, Generic, Eq)

data NoExpressionsType = IsEmpty | IsNotEmpty
  deriving (Show, Read, Generic, Eq)

data TwoExpressionsType = IsBetween | IsNotBetween
  deriving (Show, Read, Generic, Eq)

-- TODO: timchu, 12/17/15. this Ord is not exactly right. Should be the same as
-- the ordering on Evalues.
-- This is a temporary hack. Defining inequalities should be done in one place
-- and implemented for both ASValues and EValues.
-- This is also not the definitive Ord on ASValue, only used in CondFormat.
instance Ord ASValue where
  -- TODO: is this right?
  (<=) NoValue v        = (<=) (ValueI 0) v
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
  (<=) _ _ = error "Invalid ASValue comparison"


-- symbolTableLookupN takes an NExpressionType to function with N variables.
symbolTableLookup0 :: NoExpressionsType -> (ASValue -> Bool)
symbolTableLookup0 net =
  case net of
       IsEmpty    -> (==) NoValue
       IsNotEmpty -> (/=) NoValue

symbolTableLookup1 :: OneExpressionType -> (ASValue -> ASValue -> Bool)
symbolTableLookup1 oet =
  case oet of
       GreaterThan -> (>)
       Geq         -> (>=)
       LessThan    -> (<)
       Leq         -> (<=)
       Equals      -> (==)
       NotEquals   -> (/=)

-- tests if value is between a1 and a2 inclusive. Uses the Ord defined on ASValue above.
isBetween :: ASValue -> ASValue -> ASValue -> Bool
isBetween value a1 a2 = value >= min a1 a2 && max a1 a2 >= value

symbolTableLookup2 :: TwoExpressionsType -> (ASValue -> ASValue -> ASValue -> Bool)
symbolTableLookup2 tet value a1 a2 =
  case tet of
       IsBetween ->  isBetween value a1 a2
       IsNotBetween ->  not $ isBetween value a1 a2

instance ToJSON CondFormatCondition
instance FromJSON CondFormatCondition

instance ToJSON OneExpressionType
instance FromJSON OneExpressionType

instance ToJSON NoExpressionsType
instance FromJSON NoExpressionsType

instance ToJSON TwoExpressionsType
instance FromJSON TwoExpressionsType
