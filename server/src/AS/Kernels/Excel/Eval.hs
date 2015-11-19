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
convertEither :: Context -> EResult -> EitherTExec CompositeValue
convertEither _ (Left e) = return . CellValue $ ValueError (show e) "Excel"
convertEither c (Right entity) = return $ entityToComposite c entity

-- | After successful Excel eval returning an entity, convert to ASValue
-- Excel index refs are treated as 1x1 matrices, but don't treat them as lists below
entityToComposite :: Context -> EEntity -> Formatted CompositeValue 
entityToASValue c (EntityVal v) = CellValue $ eValToASValue v
entityToComposite c (EntityRef r) = case (L.refToEntity c r) of
  Left e -> CellValue $ ValueError (show e) "Excel"
  Right (EntityMatrix (EMatrix 1 1 v)) -> entityToComposite c $ EntityVal $ V.head v
  Right entity -> entityToComposite c entity
entityToComposite _ (EntityMatrix m) = case (transpose list2D) of
  [transposedCol] -> Expanding . VList . A $ transposedCol -- matches in this case iff original list2D is a vertical list
  otherwise -> Expanding . VList . M $ list2D
  where
    list2D = map (map $ orig . eValToASValue) (U.matrixTo2DList m)
-- throws away formatting in this case... awaiting your refactor, Anand. (Alex 11/11)

-- | In the Excel Error monad; parse the formula and then evaluate either as an array formula or not
evalExcel :: String -> Context -> EResult
evalExcel s context = do
  f <- C.parseFormula s
  case f of
    (ArrayFormula formula) -> L.evalArrayFormula context formula
    (SimpleFormula formula) -> L.evalFormula context formula

-- | Entire Excel eval; parse, evaluate, cast to ASValue
evaluate :: String -> ASIndex -> ValMap -> EitherTExec (Formatted CompositeValue)
evaluate s idx mp = convertEither context $ evalExcel s context
  where
    context = Context mp idx
