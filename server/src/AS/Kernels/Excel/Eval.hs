module AS.Kernels.Excel.Eval where

import AS.Types.Core
import AS.Types.Excel
import Data.List (transpose)

import AS.Kernels.Excel.Compiler as C
import AS.Kernels.Excel.Lib as L
import AS.Kernels.Excel.Util as U

import qualified Data.Map as M
import qualified Data.Vector as V

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

-- | Convert Either EError EEntity ->  Either ASExecError ASValue; lift from Excel to AS
-- | In the case of an error, return a ValueExcelError
convertEither :: Context -> EResult -> EitherTExec ASValue
convertEither _ (Left e) = return $ ValueExcelError e
convertEither c (Right entity) = return $ entityToASValue c entity

-- ::ALEX:: store additional data here
-- | After successful Excel eval returning an entity, convert to ASValue
-- | NOTE: ASValue's ValueL is column-major
-- Excel index refs are treated as 1x1 matrices, but don't treat them as lists below
entityToASValue :: Context -> EEntity -> ASValue
entityToASValue c (EntityRef r) = case (L.refToEntity c r) of
  Left e -> ValueExcelError e
  Right (EntityMatrix (EMatrix 1 1 v)) -> entityToASValue c $ EntityVal $ V.head v
  Right entity -> entityToASValue c entity
entityToASValue _ (EntityVal EBlank) = NoValue
entityToASValue _ (EntityVal EMissing) = NoValue
entityToASValue _ (EntityVal (EValueNum (Formatted (EValueD d) f))) = ValueD d
entityToASValue _ (EntityVal (EValueNum (Formatted (EValueI i) f))) = ValueI i
entityToASValue _ (EntityVal (EValueB b)) = ValueB b
entityToASValue _ (EntityVal (EValueS s)) = ValueS s
entityToASValue _ (EntityVal (EValueE s)) = ValueError s "" "" (-1)
entityToASValue _ (EntityMatrix m) = case (transpose list2D) of
  [transposedCol] -> ValueL transposedCol -- matches in this case iff original list2D is a vertical list
  otherwise -> ValueL $ map ValueL list2D
  where
    list2D = map (map $ val . toASValue) (U.matrixTo2DList m)

-- | In the Excel Error monad; parse the formula and then evaluate either as an array formula or not
evalExcel :: String -> Context -> EResult
evalExcel s context = do
  f <- C.parseFormula s
  case f of
    (ArrayFormula formula) -> L.evalArrayFormula context formula
    (SimpleFormula formula) -> L.evalFormula context formula

-- | Entire Excel eval; parse, evaluate, cast to ASValue
evaluate :: String -> ASReference -> IndValMap -> EitherTExec ASValue
evaluate s ref mp = convertEither context $ evalExcel s context
  where
    context = Context mp ref