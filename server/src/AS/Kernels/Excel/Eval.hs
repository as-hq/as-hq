module AS.Kernels.Excel.Eval where

import AS.Types.Core
import AS.Types.Excel

import AS.Kernels.Excel.Compiler as C
import AS.Kernels.Excel.Lib as L
import AS.Kernels.Excel.Util as U

import qualified Data.Map as M
import qualified Data.Vector as V


-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

-- | Convert Either EError EEntity ->  Either ASExecError ASValue; lift from Excel to AS
convertEither :: EResult -> EitherTExec ASValue
convertEither (Left e) = left $ ExecExcelError e
convertEither (Right entity) = return $ entityToASValue entity


entityToASValue :: EEntity -> ASValue 
entityToASValue (EntityRef (ERef r)) = L.dbLookup r
entityToASValue (EntityVal (EValueNum (EValueD d))) = ValueD d
entityToASValue (EntityVal (EValueNum (EValueI i))) = ValueI i
entityToASValue (EntityVal (EValueB b)) = ValueB b
entityToASValue (EntityVal (EValueS s)) = ValueS s
entityToASValue (EntityMatrix (EMatrix 1 r v)) = ValueL $ V.toList $ V.map toASValue v
-- blah


-- | In the Excel Error monad; parse the formula and then evaluate
evalExcel :: String -> ASReference -> RefValMap -> EResult
evalExcel s ref mp = do 
	let context = Context mp ref
	(formula,isArray) <- C.parseFormula s
	if isArray
		then L.evalArrayFormula context formula
		else L.evalFormula context formula


evaluate :: String -> ASReference -> RefValMap -> EitherTExec ASValue
evaluate s ref mp = convertEither $ evalExcel s ref mp