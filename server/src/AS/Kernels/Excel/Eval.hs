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

-- | Convert Either EError EEntity ->  Formatted ASValue; lift from Excel to AS
-- | In the case of an error, return a ValueExcelError
convertEither :: Context -> EResult -> Formatted ASValue
convertEither _ (Left e)       = return $ ValueExcelError e
convertEither c (Right entity) = entityToASValue c entity

-- | After successful Excel eval returning an entity, convert to ASValue
-- | NOTE: ASValue's ValueL is column-major
-- Excel index refs are treated as 1x1 matrices, but don't treat them as lists below
entityToASValue :: Context -> EEntity -> Formatted ASValue
entityToASValue c (EntityVal v) = eValToASValue v
entityToASValue c (EntityRef r) = case (L.refToEntity c r) of
  Left e -> return $ ValueExcelError e
  Right (EntityMatrix (EMatrix 1 1 v)) -> entityToASValue c $ EntityVal $ V.head v
  Right entity -> entityToASValue c entity
entityToASValue _ (EntityMatrix m) = case (transpose list2D) of 
  [transposedCol] -> return $ ValueL transposedCol -- matches in this case iff original list2D is a vertical list
  otherwise -> return $ ValueL $ map ValueL list2D
  where
    list2D = map (map $ val . eValToASValue) (U.matrixTo2DList m)
-- throws away formatting in this case... awaiting your refactor, Anand. (Alex 11/11)

-- | In the Excel Error monad; parse the formula and then evaluate either as an array formula or not
evalExcel :: String -> Context -> EResult
evalExcel s context = do
  f <- C.parseFormula s
  case f of
    (ArrayFormula formula) -> L.evalArrayFormula context formula
    (SimpleFormula formula) -> L.evalFormula context formula

-- | Entire Excel eval; parse, evaluate, cast to ASValue
evaluate :: String -> ASReference -> FormattedIndValMap -> EitherTExec (Formatted ASValue)
evaluate s ref mp = right $ convertEither context $ evalExcel s context
  where
    context = Context mp ref
