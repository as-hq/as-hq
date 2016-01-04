{-# LANGUAGE TypeFamilies, DeriveGeneric, TemplateHaskell #-}

--TODO: timchu, can clean up these imports.
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

data CondFormatRule = CondFormatRule { condFormatRuleId :: CondFormatRuleId,
                                       cellLocs :: [ASRange], 
                                       condition :: CondFormatCondition,
                                       condFormat :: CellProp } deriving (Show, Read, Generic, Eq)

-- Timchu, 12/21/15. The types have been refactored from the previous types from 12/16/15.
-- Each Condition, GreaterThanCondition, IsBetweenCondition, ... is part of an ADT called CondFormatCondition
-- Each Condition is currently an instance of
-- CustomCondition or OneExpressionCondition or NoExpressionCondition or TwoExpressionCondition

-- Each data type in typeclass OneExpressionCondition has a symbolTableLookup1 function
-- , which takes a value of that type to a (ASValue -> ASValue -> EitherTExec ASCell).
-- For example, GreaterThanCondition goes to >=.
-- Analagously, each data type in typeclass NoExpressionCondition has a symbolTableLookup0
--, which takes a value of that type to a (ASValue -> EitherTExec ASCell)
-- For example, IsEmptyCondition goes to (NoValue ==)
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

-- Note: This is MVP, text expressions and date expressions have not yet been implemented.
-- TODO: timchu, rectify names of the Custom, GreaterThan, ... constructors.

data CondFormatCondition =
  CustomCondition CustomCondition
  | IsEmptyCondition IsEmptyCondition
  | IsNotEmptyCondition IsNotEmptyCondition
  | GreaterThanCondition GreaterThanCondition
  | LessThanCondition LessThanCondition
  | GeqCondition GeqCondition
  | LeqCondition LeqCondition
  | EqualsCondition EqualsCondition
  | NotEqualsCondition NotEqualsCondition
  | IsBetweenCondition IsBetweenCondition
  | IsNotBetweenCondition IsNotBetweenCondition
  deriving (Show, Read, Generic, Eq)


data CustomCondition = Custom ASExpression deriving (Show, Read, Generic, Eq)
data IsEmptyCondition = IsEmpty deriving (Show, Read, Generic, Eq)
data IsNotEmptyCondition = IsNotEmpty deriving (Show, Read, Generic, Eq)
data GreaterThanCondition = GreaterThan ASExpression  deriving (Show, Read, Generic, Eq)
data LessThanCondition = LessThan ASExpression  deriving (Show, Read, Generic, Eq)
data GeqCondition = Geq ASExpression  deriving (Show, Read, Generic, Eq)
data LeqCondition = Leq ASExpression  deriving (Show, Read, Generic, Eq)
data EqualsCondition = Equals ASExpression  deriving (Show, Read, Generic, Eq)
data NotEqualsCondition = NotEquals ASExpression  deriving (Show, Read, Generic, Eq)
data IsBetweenCondition = IsBetween ASExpression ASExpression deriving (Show, Read, Generic, Eq)
data IsNotBetweenCondition = IsNotBetween ASExpression ASExpression deriving (Show, Read, Generic, Eq)

-------------------------------------------------------------------------------------------------------------------------
-- Instances

-- | TODO: timchu, 12/21/15. There is a fair amount of reptition in the code in 
-- the Inequality Conditions, the Between Conditions, etc...

-- Establishes an instance of Condition for each possible CondFormatCondition
instance Condition CondFormatCondition where
  checker (CustomCondition x) = checkerCustom x
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

-- | Establishes CustomCondition as an instance of CustomExpressionCondition.
-- NOTE: only CustomCondition is an instance of CustomExpressionCondition,
-- but this logic is present to mirror the logic of the other expressions.
instance CustomExpressionCondition CustomCondition where
  getCustomXp (Custom xp) = xp

-- | Establishes IsEmptyCondition, IsNotEmptyCondition as instances of NoExpressionCondition.
-- Establishes the symbolTableLookup corresponding to the condition types IsEmpty and IsNotEmpty.
-- checkerNone is automatically defined given symbolTableLookup0.
instance NoExpressionCondition IsEmptyCondition where
  symbolTableLookup0 IsEmpty = (NoValue == )
instance NoExpressionCondition IsNotEmptyCondition where
  symbolTableLookup0 IsNotEmpty = (NoValue /= )

-- | Establishes the Inequality Conditions as instances of OneExpressionCondition.
-- Establishes the symbolTableLookup corresponding to the inequality condition types.
-- chckerOne is automatically defined given getXp and symbolTableLookup1
instance OneExpressionCondition GreaterThanCondition where
  symbolTableLookup1 (GreaterThan _) = (>)
  getXp (GreaterThan xp) = xp
instance OneExpressionCondition LessThanCondition where
  symbolTableLookup1 (LessThan _) = (<)
  getXp (LessThan xp) = xp
instance OneExpressionCondition GeqCondition where
  symbolTableLookup1 (Geq _) = (>=)
  getXp (Geq xp) = xp
instance OneExpressionCondition LeqCondition where
  symbolTableLookup1 (Leq _) = (<=)
  getXp (Leq xp) = xp
instance OneExpressionCondition EqualsCondition where
  symbolTableLookup1 (Equals _) = (==)
  getXp (Equals xp) = xp
instance OneExpressionCondition NotEqualsCondition where
  symbolTableLookup1 (NotEquals _) = (/=)
  getXp (NotEquals xp) = xp

-- | Establishes IsBetweenCondition, IsNotBetweenCondition as instances of TwoExpressionCondition.
-- Establishes the symbolTableLookup corresponding to the condition types IsBetween and IsNotBetween.
-- checkerTwo is automatically defined given symbolTableLookup2, getFstXp, getSndXp.
instance TwoExpressionCondition IsBetweenCondition where
  symbolTableLookup2 (IsBetween _ _ ) = isBetween
  getFstXp (IsBetween a _) = a
  getSndXp (IsBetween _ b) = b
instance TwoExpressionCondition IsNotBetweenCondition where
  symbolTableLookup2 (IsNotBetween _ _) v v1 v2 = not $ isBetween  v v1 v2
  getFstXp (IsNotBetween a _) = a
  getSndXp (IsNotBetween _ b) = b


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


-- | Definitions of the Condition Typeclasses:

-- symbolTableLookupN takes an NExpressionType to function with N variables.
-- Checker is a function taking in an instance of NExpressionCondition,
-- an ASValue of a cell that the conditional format is evaluated on,
-- and an eval function (ASExpression -> EitherTExec ASValue),
-- and returns a bool to determine whether the cond format condition has been satisfied or not.

class Condition a where
  checker :: a -> ASValue -> (ASExpression -> EitherTExec ASValue) -> EitherTExec Bool

-- TODO: timchu, this type class is not necessary since only customExpressionCondition is an instance of it,
-- but it is here so that the code for CustomExpression matches the rest of the logic.
class CustomExpressionCondition a where
  checkerCustom :: a -> ASValue -> (ASExpression -> EitherTExec ASValue) -> EitherTExec Bool
  getCustomXp :: a -> ASExpression
  checkerCustom s _ evalXp = do
    v <- evalXp $ getCustomXp s
    if v == ValueB True
       then return True
       else return False

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

type CondFormatRuleDiff = Diff CondFormatRule
type CondFormatRuleUpdate = Update CondFormatRule CondFormatRuleId

instance HasKey CondFormatRule where
  type KeyType CondFormatRule = CondFormatRuleId
  key = condFormatRuleId

asToFromJSON ''CondFormatRule
asToFromJSON ''CondFormatCondition
asToFromJSON ''CondFormatRuleDiff
asToFromJSON ''CondFormatRuleUpdate
asToFromJSON ''CustomCondition
asToFromJSON ''GreaterThanCondition
asToFromJSON ''LessThanCondition
asToFromJSON ''GeqCondition
asToFromJSON ''LeqCondition
asToFromJSON ''EqualsCondition
asToFromJSON ''NotEqualsCondition
asToFromJSON ''IsEmptyCondition
asToFromJSON ''IsNotEmptyCondition
asToFromJSON ''IsBetweenCondition
asToFromJSON ''IsNotBetweenCondition

deriveSafeCopy 1 'base ''CondFormatRule
deriveSafeCopy 1 'base ''CondFormatCondition
deriveSafeCopy 1 'base ''CustomCondition
deriveSafeCopy 1 'base ''GreaterThanCondition
deriveSafeCopy 1 'base ''LessThanCondition
deriveSafeCopy 1 'base ''GeqCondition
deriveSafeCopy 1 'base ''LeqCondition
deriveSafeCopy 1 'base ''EqualsCondition
deriveSafeCopy 1 'base ''NotEqualsCondition
deriveSafeCopy 1 'base ''IsEmptyCondition
deriveSafeCopy 1 'base ''IsNotEmptyCondition
deriveSafeCopy 1 'base ''IsBetweenCondition
deriveSafeCopy 1 'base ''IsNotBetweenCondition
