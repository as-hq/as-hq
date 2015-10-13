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

-- | After successful Excel eval returning an entity, convert to ASValue
-- | NOTE: ASValue's ValueL is column-major
entityToASValue :: Context -> EEntity -> ASValue 
entityToASValue c (EntityRef r) = case (L.refToEntity c r) of
	Left e -> ValueExcelError e
	Right entity -> entityToASValue c entity
entityToASValue _ (EntityVal (EValueNum (EValueD d))) = ValueD d
entityToASValue _ (EntityVal (EValueNum (EValueI i))) = ValueI i
entityToASValue _ (EntityVal (EValueB b)) = ValueB b
entityToASValue _ (EntityVal (EValueS s)) = ValueS s
entityToASValue _ (EntityMatrix m) = case list2D of
	[row] -> ValueL row
	otherwise -> ValueL $ map ValueL list2D
	where 
		list2D' = map (map toASValue) (U.matrixTo2DList m)
		list2D = transpose list2D'

-- | In the Excel Error monad; parse the formula and then evaluate either as an array formula or not
evalExcel :: String -> Context -> EResult
evalExcel s context = do 
	(formula,isArray) <- C.parseFormula s
	if isArray
		then L.evalArrayFormula context formula
		else L.evalFormula context formula

-- | Entire Excel eval; parse, evaluate, cast to ASValue
evaluate :: String -> ASReference -> RefValMap -> EitherTExec ASValue
evaluate s ref mp = convertEither context $ evalExcel s context
	where
		context = Context mp ref
