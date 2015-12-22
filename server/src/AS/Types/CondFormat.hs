{-# LANGUAGE DeriveGeneric #-}

--TODO: timchu, can clean up these imports.
module AS.Types.CondFormat where

import AS.Types.DB (ASCommit)
import AS.Types.Cell
import AS.Types.Eval
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

instance ToJSON CondFormatCondition
instance FromJSON CondFormatCondition
instance Serialize CondFormatRule
instance Serialize CondFormatCondition


data CondFormatRule = CondFormatRule { cellLocs :: [ASRange],
                                       condition :: CondFormatCondition,
                                       condFormat :: CellProp } deriving (Show, Read, Generic, Eq)

-- Timchu, 12/21/15. The types have been refactored from the previous types from 12/16/15.
-- Each Condition, GreaterThan, IsBetween, ... is part of an ADT called CondFormatCondition
-- Each Condition is currently an instance of OneExpressionCondition or NoExpressionCondition or TwoExpressionCondition
-- Note: This is MVP, text expressions and date expressions have not yet been implemented.
-- Each data type in typeclass OneExpressionCondition has a symbolTableLookup1 function
-- , which takes a value of that type to a (ASValue -> ASValue -> EitherTExec ASCell).
-- For example, GreaterThan goes to >=.
-- Analagously, each data type in typeclass NoExpressionCondition has a symbolTableLookup0
--, which takes a value of that type to a (ASValue -> EitherTExec ASCell)
-- For example, IsEmpty goes to (NoValue ==)
--
-- Each data type in OneExpressionCondition has a checkerOne function based on the symbolTableLookup
-- and the expressions entered into the conditional formatting criterion that takes in a
--    - ASValue -> EitherTExec Bool (an eval function that is passed in),
--    - Value (value of the cell that is being checked with the condition)
--    - A member of that data type (e.g. GreaterThan (Expression "=A1+1" Excel))
--    These give enough information to check whether Value satisfies the 
-- conditional formatting condition in question, given the eval funtion passed in.
-- Likewise for NoExpressionCondition and  TwoExpressionCondition.
-- CondFormatCondition is an instance of Condition, which has a checker
-- that evaluates to CheckerNone or CheckerOne or CheckerTwo, depending on the Condition.

data CondFormatCondition =
  GreaterThanCondition GreaterThan
  | LessThanCondition LessThan
  | GeqCondition Geq
  | LeqCondition Leq
  | EqualsCondition Equals
  | NotEqualsCondition NotEquals
  | IsEmptyCondition IsEmpty
  | IsNotEmptyCondition IsNotEmpty
  | IsBetweenCondition IsBetween
  | IsNotBetweenCondition IsNotBetween
  deriving (Show, Read, Generic, Eq)


data GreaterThan = GreaterThan ASExpression  deriving (Show, Read, Generic, Eq)
data LessThan = LessThan ASExpression  deriving (Show, Read, Generic, Eq)
data Geq = Geq ASExpression  deriving (Show, Read, Generic, Eq)
data Leq = Leq ASExpression  deriving (Show, Read, Generic, Eq)
data Equals = Equals ASExpression  deriving (Show, Read, Generic, Eq)
data NotEquals = NotEquals ASExpression  deriving (Show, Read, Generic, Eq)
data IsEmpty = IsEmpty deriving (Show, Read, Generic, Eq)
data IsNotEmpty = IsNotEmpty deriving (Show, Read, Generic, Eq)
data IsBetween = IsBetween ASExpression ASExpression deriving (Show, Read, Generic, Eq)
data IsNotBetween = IsNotBetween ASExpression ASExpression deriving (Show, Read, Generic, Eq)

-- | TODO: timchu, 12/21/15. There is a fair amount of reptition in the code in 
-- all the Inequality Conditions, the Between Conditions, etc...
-- I assume there's a better way to do this.

-- Establishes an instance of condition for each possible CondFormatCondition
instance Condition CondFormatCondition where
  checker (GreaterThanCondition x) = checkerOne x
  checker (LessThanCondition x) = checkerOne x
  checker (GeqCondition x) = checkerOne x
  checker (LeqCondition x) = checkerOne x
  checker (EqualsCondition x) = checkerOne x
  checker (NotEqualsCondition x) = checkerOne x
  checker (IsEmptyCondition x) = checkerNone x
  checker (IsNotEmptyCondition x) = checkerNone x
  checker (IsBetweenCondition x) = checkerTwo x
  checker (IsNotBetweenCondition x) = checkerTwo x
  --TODO: fill in more functions.

-- | Establishes the Inequality types as instances of OneExpressionCondition.
-- Establishes the symbols corresponding to the inequality condition types.
-- chckerOne is automatically defined given getXp and symbolTableLookup1
instance OneExpressionCondition GreaterThan where
  symbolTableLookup1 (GreaterThan _) = (>)
  getXp (GreaterThan xp) = xp
instance OneExpressionCondition LessThan where
  symbolTableLookup1 (LessThan _) = (<)
  getXp (LessThan xp) = xp
instance OneExpressionCondition Geq where
  symbolTableLookup1 (Geq _) = (>=)
  getXp (Geq xp) = xp
instance OneExpressionCondition Leq where
  symbolTableLookup1 (Leq _) = (<=)
  getXp (Leq xp) = xp
instance OneExpressionCondition Equals where
  symbolTableLookup1 (Equals _) = (==)
  getXp (Equals xp) = xp
instance OneExpressionCondition NotEquals where
  symbolTableLookup1 (NotEquals _) = (/=)
  getXp (NotEquals xp) = xp

-- | Establishes IsEmpty, IsNotEmpty as instances of NoExpressionCondition.
-- Establishes the symbols corresponding to the condition types IsEmpty and IsNotEmpty.
-- checkerNone is automatically defined given symbolTableLookup0.
instance NoExpressionCondition IsEmpty where
  symbolTableLookup0 IsEmpty = (NoValue == )
instance NoExpressionCondition IsNotEmpty where
  symbolTableLookup0 IsNotEmpty = (NoValue /= )

-- | Establishes IsBetween, IsNotBetween as instances of TwoExpressionCondition.
-- Establishes the symbols corresponding to the condition types IsBetween and IsNotBetween.
-- checkerTwo is automatically defined given symbolTableLookup2, getFstXp, getSndXp.
instance TwoExpressionCondition IsBetween where
  symbolTableLookup2 (IsBetween _ _ ) = isBetween
  getFstXp (IsBetween a _) = a
  getSndXp (IsBetween _ b) = b
instance TwoExpressionCondition IsNotBetween where
  symbolTableLookup2 (IsNotBetween _ _) v v1 v2 = not $ isBetween  v v1 v2
  getFstXp (IsNotBetween a _) = a
  getSndXp (IsNotBetween _ b) = b


-- | Defines the ordering used in GreaterThan, IsBetween, ...
-- This ordering is only used in CondFormat thus far. Hence it is in this file.
-- TODO: timchu, 12/17/15. this Ord is not exactly right. Should be the same as
-- the ordering on Evalues. This has not been vetted for completeness or tested
-- for correctness.
-- This is a temporary hack. Defining these ineqs should be done in one place
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


-- | Definitions of the Condition Typeclasses:

-- symbolTableLookupN takes an NExpressionType to function with N variables.
-- Checker is a function taking in an instance of NExpressionCondition,
-- an ASValue of a cell that the conditional format is evaluated on, and an eval function (ASExpression -> EitherTExec ASValue), which returns a bool to determine whether the cond format condition has been satisfied or not.

class Condition a where
  checker :: a -> ASValue -> (ASExpression -> EitherTExec ASValue) -> EitherTExec Bool

class NoExpressionCondition a where
  symbolTableLookup0 :: a -> (ASValue -> Bool)
  checkerNone :: a -> ASValue -> (ASExpression -> EitherTExec ASValue) -> EitherTExec Bool
  -- TODO: Timchu, 12/21. This implementation seems suboptimal. Don't need to pass in an Eval for this case.
  checkerNone s val _ =
    return $ (symbolTableLookup0 s) val

class OneExpressionCondition a where
  symbolTableLookup1 :: a -> (ASValue -> ASValue -> Bool)
  getXp :: a -> ASExpression
  checkerOne :: a -> ASValue -> (ASExpression -> EitherTExec ASValue) -> EitherTExec Bool
  checkerOne s val evalXp = do
    val1 <- evalXp $ getXp s
    return $ (symbolTableLookup1 s) val val1

class TwoExpressionCondition a where
  symbolTableLookup2 :: a -> (ASValue -> ASValue -> ASValue -> Bool)
  getFstXp :: a -> ASExpression
  getSndXp :: a -> ASExpression
  checkerTwo :: a -> ASValue -> (ASExpression -> EitherTExec ASValue) -> EitherTExec Bool
  checkerTwo s val evalXp = do
    [val1, val2] <- mapM evalXp [getFstXp s, getSndXp s]
    return $ (symbolTableLookup2 s) val val1 val2


instance ToJSON GreaterThan
instance FromJSON GreaterThan
instance Serialize GreaterThan

instance ToJSON LessThan
instance FromJSON LessThan
instance Serialize LessThan

instance ToJSON Geq
instance FromJSON Geq
instance Serialize Geq

instance ToJSON Leq
instance FromJSON Leq
instance Serialize Leq

instance ToJSON Equals
instance FromJSON Equals
instance Serialize Equals

instance ToJSON NotEquals
instance FromJSON NotEquals
instance Serialize NotEquals

instance ToJSON IsEmpty
instance FromJSON IsEmpty
instance Serialize IsEmpty

instance ToJSON IsNotEmpty
instance FromJSON IsNotEmpty
instance Serialize IsNotEmpty

instance ToJSON IsBetween
instance FromJSON IsBetween
instance Serialize IsBetween

instance ToJSON IsNotBetween
instance FromJSON IsNotBetween
instance Serialize IsNotBetween
