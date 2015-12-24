{-# LANGUAGE DeriveGeneric #-}

-- #needsrefactor -- should be renamed to ExcelErrors

module AS.Types.Errors where

import AS.Types.Locations

import GHC.Generics
import Data.Aeson

-- memory
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import Control.Monad.Trans.Either

-- | TODO: create custom show instance that takes REF/NA/VALUE etc into account
data EError =
  ExcelSyntaxError |
  EmptyMatrix String |
  NotFunction String |
  TooManyArgs String |
  ArrayConstantDim |
  CannotIntersectRefs |
  CannotScalarizeArrConst |
  EmptyArrayConstant |
  CannotNormalizeDimensions |
  ScalarizeIntersectionError ASReference ASReference |
  CannotConvertToExcelValue ASReference |
  InvalidArrayConstEntity |
  ArrayFormulaUnMappable | -- one of the arguments returned a reference or matrix, not a value
  NumArgs {fNameNA :: String, expectedNumArgs :: Int, actualNumArgs :: Int} |
  RequiredArgMissing {fNameRAM :: String, argNumRAM :: Int} |
  ArgType {fNameAT :: String, argNumAT :: Int, expectedType :: String, actualType :: String} |
  Default String |
  VAL String |
  REF String |
  NA String  |
  NUM String |
  DIV0 | 
  NegExpBaseWithFloatingExp | 
  SqrtNegative Double |
  ZeroToTheZero
  deriving (Read, Eq, Ord, Generic)

data EErrorType = NullErr | Div0Err | ValErr | RefErr | NameErr | NumErr | NAErr | GetErr | OtherErr

instance Show EErrorType where
  show NullErr = "#NULL!"
  show Div0Err = "#DIV/0!"
  show ValErr = "#VALUE!"
  show RefErr = "#REF!"
  show NameErr = "#NAME?"
  show NumErr = "#NUM!"
  show NAErr = "#N/A"
  show GetErr = "GETTING_DATA"
  show OtherErr = "#N/A"

instance Show EError where
  show e = "Excel " ++ (show errorType) ++ " error, possibly due to " ++ detail
    where (errorType, detail) = getExcelErrorType e

getExcelErrorType :: EError -> (EErrorType, String)
getExcelErrorType ExcelSyntaxError  = (NAErr, "an error in syntax")
getExcelErrorType (EmptyMatrix s) = (ValErr, "an empty matrix: " ++ s)
getExcelErrorType (SqrtNegative d) = (NumErr, "trying to take the square root of a negative number, " ++ (show d))
getExcelErrorType (NotFunction s) = (NameErr, "a function, " ++ s ++ ", that doesn't exist")
getExcelErrorType (TooManyArgs s) = (NAErr, "giving too many arguments to " ++ s)
getExcelErrorType ArrayConstantDim = (ValErr, "incorrect array constant dimensionality")
getExcelErrorType CannotIntersectRefs = (ValErr, "an inability to intersect references")
getExcelErrorType CannotScalarizeArrConst = (ValErr, "an inability to scalarize an array constant")
getExcelErrorType EmptyArrayConstant = (ValErr, "an empty array constant")
getExcelErrorType CannotNormalizeDimensions = (ValErr, "being unable to normalize dimensions")
getExcelErrorType (ScalarizeIntersectionError r1 r2) = (ValErr, "an inability to intersect references")
getExcelErrorType (CannotConvertToExcelValue ref) = 
  (NAErr, "an inability to convert a value at " ++ (show ref) ++ " to an Excel value (check languages)")
getExcelErrorType InvalidArrayConstEntity = (ValErr, "an invalid array constant")
getExcelErrorType ArrayFormulaUnMappable = (ValErr, "an array formula not working")
getExcelErrorType (NumArgs fName expectedArgs actualArgs) = 
  (ValErr, "a function, " ++ fName ++ ", which expected " ++ (show expectedArgs) ++ " arguments, but got " ++ (show actualArgs) ++ " arguments instead")
getExcelErrorType (RequiredArgMissing fName argNum) = 
  (ValErr, "a function, " ++ fName ++ ", which is missing required argument number " ++ (show argNum))
getExcelErrorType (ArgType fName argNum expected actual) = 
  (ValErr, "a function, " ++ fName ++ ", which expected argument number " ++ (show argNum) ++ " to be a " ++ expected ++ ", not " ++ actual)
getExcelErrorType (Default s) = (NAErr,s)
getExcelErrorType (VAL s) = (ValErr,s)
getExcelErrorType (REF s) = (RefErr,s)
getExcelErrorType (NA s)  = (NAErr,s)
getExcelErrorType (NUM s) = (NumErr,s)
getExcelErrorType DIV0 = (Div0Err, "dividing by zero (come on now!)")
getExcelErrorType NegExpBaseWithFloatingExp = (NumErr, "a negative exponent base with a floating exponent")
getExcelErrorType ZeroToTheZero = (NumErr, "evaluating 0^0")

data ASExecError =
    Timeout
  | WillNotEvaluate
  | EvaluationError {evalErrorDesc :: String}
  | DependenciesLocked {lockUserId :: ASUserId}
  | DBNothingException {badLocs :: [ASIndex]}
  | DecoupleAttempt
  | UnknownGraphError 
  | CircularDepError {badLoc :: ASIndex}
  | IndexOfPointerNonExistant 
  | PointerToNormalCell
  | CondFormattingError String
  | NetworkDown
  | RuntimeEvalException
  | ResourceLimitReached
  | InsufficientPermissions
  | NonUniqueIdentifier
  | ParseError
  | ExpressionNotEvaluable
  | ExecError
  | SyntaxError
  | HighDimensionalValue
  | APIError
  deriving (Show, Read, Eq, Generic)

type EitherTExec = EitherT ASExecError IO

instance ToJSON ASExecError
instance FromJSON ASExecError

instance FromJSON EError
instance ToJSON EError

-- memory region exposure instances for R value unboxing
instance NFData EError              where rnf = genericRnf