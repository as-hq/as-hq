module AS.Kernels.Excel.Eval where

import Data.List (transpose)
import Control.Lens hiding (Context)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Database.Redis (Connection)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.Vector as V

import AS.Prelude
import AS.Types.Cell
import AS.Types.Eval
import AS.Types.Excel
import AS.Types.Formats
import AS.Types.User

import AS.Kernels.Excel.Compiler as C
import AS.Kernels.Excel.Lib as L
import AS.Kernels.Excel.Util as U

import AS.DB.Users (getUserSheets)

-- | Convert Either EError EEntity ->  Formatted ASValue; lift from Excel to AS
-- | In the case of an error, return a ValueExcelError
convertEither :: Context -> EResult -> Formatted CompositeValue
convertEither _ (Left e) = return . CellValue $ ValueError (show e) "Excel"
convertEither c (Right entity) = entityToComposite c entity

-- | After successful Excel eval returning an entity, convert to ASValue
-- | NOTE: ASValue's ValueL is column-major
-- Excel index refs are treated as 1x1 matrices, but don't treat them as lists below
entityToComposite :: Context -> EEntity -> Formatted CompositeValue
entityToComposite c (EntityVal v) = formattedValToComposite $ eValToASValue v
  where formattedValToComposite (Formatted v f) = Formatted (CellValue v) f
entityToComposite c (EntityRef r) = case (L.refToEntity c r) of
  Left e -> return . CellValue $ ValueError (show e) "Excel"
  Right (EntityMatrix (EMatrix 1 1 v)) -> entityToComposite c $ EntityVal $ V.head v
  Right entity -> entityToComposite c entity
entityToComposite _ (EntityMatrix m) = case (transpose list2D) of 
  [transposedCol] -> return $ Expanding . VList . A $ transposedCol -- matches in this case iff original list2D is a vertical list
  otherwise -> return $ Expanding . VList . M $ list2D
  where
    list2D = map (map $ view orig . eValToASValue) (U.matrixTo2DList m)
-- throws away formatting in this case... awaiting your refactor, Anand. (Alex 11/11)

-- | In the Excel Error monad; parse the formula and then evaluate either as an array formula or not
evalExcel :: String -> Context -> EitherT EError IO EEntity
evalExcel s context = do
  f <- hoistEither $ C.parseFormula $ BC.pack s
  case f of
    ArrayFormula formula -> L.evalArrayFormula context formula
    SimpleFormula formula -> L.evalFormula context formula

-- | Entire Excel eval; parse, evaluate, cast to ASValue
-- Excel doesn't have print statements, so the display value of EvalResult is always Nothing
evaluate :: Connection -> ASUserId -> String -> ASIndex -> CellMap -> IO (Formatted EvalResult)
evaluate conn uid s idx mp =  do
  sheets <- getUserSheets conn uid
  let context = Context mp idx sheets conn
  eResult <- runEitherT $ evalExcel s context
  let val = convertEither context eResult
  return $ EvalResult <$> val <*> return Nothing
