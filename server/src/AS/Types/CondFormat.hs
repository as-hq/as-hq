{-# LANGUAGE DeriveGeneric #-}

--TODO: timchu, can clean up these imports.
module AS.Types.CondFormat where

import AS.Types.DB (ASCommit)
import AS.Types.Cell
import AS.Types.Locations
import AS.Types.CellProps
-- apparently used for FromJSON on ADTs
import qualified Data.HashMap.Strict as HML
import qualified Data.Text.Lazy as TL
import Data.List

import GHC.Generics
import Data.Aeson hiding (Success)
import Data.Aeson.Types
import Data.Serialize (Serialize)
import qualified Data.Text as T


instance ToJSON CondFormatRule
instance FromJSON CondFormatRule

instance ToJSON CondFormatCondition where
  toJSON = makeObj
    where
      makeObj :: CondFormatCondition -> Value
      makeObj cfc =
        case cfc of
             NoExpressionsCondition t -> object ["tag" .= show t]
             OneExpressionCondition t xp -> object ["tag" .= show t, "expression" .= xp]
             TwoExpressionsCondition t xp1 xp2 -> object ["tag" .= show t, "expressions" .= [xp1, xp2]]


-- Example of a CondFormatObject is
-- {tag: "IsBetween"
-- expressions: [ASExpression , ASExpression]}
instance FromJSON CondFormatCondition where
 parseJSON (Object v) =
   case HML.lookup "tag" v of
        Just (String "CustomExpressionCondition") -> CustomExpressionCondition <$> v.:"expression"
        Just (String "GreaterThan") -> oneExpressionParser v
        Just (String "Equals") -> oneExpressionParser v
        Just (String "LessThan") -> oneExpressionParser v
        Just (String "NotEquals") -> oneExpressionParser v
        Just (String "Leq") -> oneExpressionParser v
        Just (String "Geq") -> oneExpressionParser v
        Just (String "IsBetween") -> twoExpressionsParser v
        Just (String "IsNotBetween") -> twoExpressionsParser v
        Just (String "IsEmpty") -> noExpressionsParser v
        Just (String "IsNotEmpty") -> noExpressionsParser v
        where
              noExpressionsParser  :: Object -> Parser CondFormatCondition
              noExpressionsParser s = NoExpressionsCondition <$> s .: "tag"
              oneExpressionParser :: Object -> Parser CondFormatCondition
              oneExpressionParser s =  OneExpressionCondition <$> s .: "tag" <*> s .: "expression"
              twoExpressionsParser  :: Object -> Parser CondFormatCondition
              twoExpressionsParser s = do 
                list <- s .: "expressions"
                let e1 = head list
                    e2 = last list
                TwoExpressionsCondition <$> s .: "tag" <*> return e1 <*> return e2
                -- TwoExpressionsCondition <$> s .: "tag" <*> s .: "expressionOne" <*> s .: "expressionTwo"
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

--data CondFormatCondition = GT GreaterThan | IE IsEmpty -- | CE CustomExpression | IB IsBetween
--data GreaterThan = GreaterThan ASExpression
--
--data OneXPTag = GreaterThan
--data NoXPTag = IsEmpty
data CondFormatCondition =
    CustomExpressionCondition ASExpression
  | NoExpressionsCondition NoExpressionsType
  | OneExpressionCondition OneExpressionType ASExpression
  | TwoExpressionsCondition TwoExpressionsType ASExpression ASExpression
   deriving (Show, Read, Generic, Eq)
--
---- BEGIN TIM REFACTOR
--instance ConditionOne GreaterThan where
--symbolTableLookUp1 (GreaterThan _) = (>=)
--getXp (GreaterThan xp) = xp
--
--instance Condition GreaterThan where
--checker = checkerOne
--
--instance ConditionOne GreaterThan where
--symbolTableLookUp1 GreaterThan _ = (>=)
--getXp GreaterThan s = s
--
--instance ConditionZero IsEmpty where
--symbolTableLookup0 IsEmpty = ((==) NoValue)
--
--instance Condition IsEmpty
--checker = CheckerZero
--
--
--
--class Condition a where
--  checker :: a -> ASValue -> (ASExpression -> EitherTExec ASValue) -> EitherTExec Bool
--
--class Condition a => NoExpressionCondition a where
--  symbolTableLookup1 :: a -> (ASValue -> ASValue -> Bool)
--  checkerZero :: a -> ASValue -> EitherTExec Bool
--  checkerZero s val = do
--    return $ (symbolTableLookup1 s) val
--
--class Condition a => OneExpressionCondition a where
--  symbolTableLookup1 :: a -> (ASValue -> ASValue -> Bool)
--  getXp :: a -> ASExpression
--  checkerOne :: a -> ASValue -> (ASExpression -> EitherTExec ASValue) -> EitherTExec Bool
--  checkerOne s evalXp val = do
--    val1 <- evalXp $ getExpression a
--    return $ (symbolTableLookup1 s) val val1
--
--class Condition a => TwoExpressionCondition a where
--  symbolTableLookup2 :: a -> (ASValue -> ASValue -> ASValue -> Bool)
--  getFstXp :: a -> EitherTExec ASValue
--  getSndXp :: a -> EitherTExec ASValue
--  checkerTwo :: a -> ASValue -> (ASExpression -> EitherTExec ASValue) EitherTExec Bool
--  checkerTwo s val = do
--    val1 <- evalXp $ getFstXp t
--    val2 <- evalXp $ getSndXp t
--    return $ (symbolTableLookup2 s) val val1 val2

data OneExpressionType = GreaterThan | Equals | Geq | Leq | LessThan | NotEquals
  deriving (Show, Read, Generic, Eq)

data NoExpressionsType = IsEmpty | IsNotEmpty
  deriving (Show, Read, Generic, Eq)

data TwoExpressionsType = IsBetween | IsNotBetween
  deriving (Show, Read, Generic, Eq)

-- TODO: timchu, 12/17/15. this Ord is not exactly right. Should be the same as
-- the ordering on Evalues. This has not been vetted for completeness or tested
-- for correctness.
-- This is a temporary hack. Defining these ineqs should be done in one place
-- and implemented for both ASValues and EValues.
-- This is also not the definitive Ord on ASValue, only used in CondFormat.
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

instance ToJSON OneExpressionType
instance FromJSON OneExpressionType

instance ToJSON NoExpressionsType
instance FromJSON NoExpressionsType

instance ToJSON TwoExpressionsType
instance FromJSON TwoExpressionsType
