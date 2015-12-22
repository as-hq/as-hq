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

-- Custom ToJSON and FromJSON on CondFormatConditions becuase the frontend and backend types differ.
instance ToJSON CondFormatCondition where
instance FromJSON CondFormatCondition where
instance Serialize CondFormatRule
instance Serialize CondFormatCondition


data CondFormatRule = CondFormatRule { cellLocs :: [ASRange],
                                       condition :: CondFormatCondition,
                                       condFormat :: CellProp } deriving (Show, Read, Generic, Eq)

data CondFormatCondition =
  GreaterThanCondition GreaterThan
  | IsEmptyCondition IsEmpty
  deriving (Show, Read, Generic, Eq)

data GreaterThan = GreaterThan ASExpression  deriving (Show, Read, Generic, Eq)
data IsEmpty = IsEmpty deriving (Show, Read, Generic, Eq)

instance ToJSON GreaterThan
instance FromJSON GreaterThan
instance ToJSON IsEmpty
instance FromJSON IsEmpty
instance Serialize IsEmpty
instance Serialize GreaterThan


-- Timchu, 12/21/15. The types have been refactored from the previous types from 12/16/15.
-- ConditionalFormattingConditions is split into all possible conditions.
-- Each condition is an instance  of conditionNone, conditionOne, conditionTwo
-- depending on what the input type to the conditional formatting condition is.
-- Note: This is MVP, text expressions and date expressions have not yet been
-- implemented.
-- Each data type in typeclass ConditionOne has a symbolTableLookup1 function 
-- that takes a value of that type to a (ASValue -> ASValue -> EitherTExec ASCell).
-- For example, GreaterThan goes to >=.
--
-- Each data type in ConditionOne then has a checkerOne function that takes in
-- an 
--    ASValue -> EitherTExec Bool (essentially, an eval function that is passed in),
--    Value (value of the cell that is being checked with the condition)
--    A member of that data type
--    These give enough information to take a member of the data type to the appropriate bool.
-- An analagous thing occurs for ConditionNone and  ConditionTwo.
-- Then CondFormatCondition is an instance of Condition, which has a checker
-- that evaluates to CheckerNone or CheckerOne or CheckerTwo, depending on the Condition.

instance ConditionOne GreaterThan where
  symbolTableLookup1 (GreaterThan _) = (>)
  getXp (GreaterThan xp) = xp

-- | TODO: timchu, 12/21/15. I assume there's a better way to do this.
-- TODO: timchu, 12/21/15. Figure out how to case within instances!
instance Condition CondFormatCondition where
  checker (GreaterThanCondition x) = checkerOne x
  checker (IsEmptyCondition x) = checkerNone x
  --TODO: fill in more functions.

instance ConditionNone IsEmpty where
  symbolTableLookup0 IsEmpty = (NoValue == )

class Condition a where
  checker :: a -> ASValue -> (ASExpression -> EitherTExec ASValue) -> EitherTExec Bool

class ConditionNone a where
  symbolTableLookup0 :: a -> (ASValue -> Bool)
  checkerNone :: a -> ASValue -> (ASExpression -> EitherTExec ASValue) -> EitherTExec Bool
  -- TODO: Timchu, 12/21. This implementation seems suboptimal. Don't need to pass in an Eval for this case.
  checkerNone s val _ =
    return $ (symbolTableLookup0 s) val

class ConditionOne a where
  symbolTableLookup1 :: a -> (ASValue -> ASValue -> Bool)
  getXp :: a -> ASExpression
  checkerOne :: a -> ASValue -> (ASExpression -> EitherTExec ASValue) -> EitherTExec Bool
  checkerOne s val evalXp = do
    val1 <- evalXp $ getXp s
    return $ (symbolTableLookup1 s) val val1

class ConditionTwo a where
  symbolTableLookup2 :: a -> (ASValue -> ASValue -> ASValue -> Bool)
  getFstXp :: a -> ASExpression
  getSndXp :: a -> ASExpression
  checkerTwo :: a -> ASValue -> (ASExpression -> EitherTExec ASValue) -> EitherTExec Bool
  checkerTwo s val evalXp = do
    [val1, val2] <- mapM evalXp [getFstXp s, getSndXp s]
    return $ (symbolTableLookup2 s) val val1 val2

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
-- tests if value is between a1 and a2 inclusive. Uses the Ord defined on ASValue above.
isBetween :: ASValue -> ASValue -> ASValue -> Bool
isBetween value a1 a2 = value >= min a1 a2 && max a1 a2 >= value
