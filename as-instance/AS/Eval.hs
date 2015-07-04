module AS.Eval where

import Import hiding (writeFile)
import qualified Prelude as P
import Control.Applicative
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T
import AS.Eval.Lang
import AS.Types
import AS.DB
import AS.Parsing.In
import AS.Parsing.Out
import AS.Parsing.Common
import System.IO                                       
import System.Process   

-- REGEX DELETE
import qualified AS.Parsing.Regex as R

-----------------------------------------------------------------------------------------------------------------------
-- File Interpolation (see Lang for details)

evalExpression :: ASLocation -> Map ASLocation ASValue -> ASExpression -> Handler ASValue
evalExpression loc dict expr =
  case expr of
    Expression _ _ -> evalCode loc dict expr  
    Reference _ _ -> evalRef loc dict expr  

evalCode :: ASLocation -> Map ASLocation ASValue -> ASExpression -> Handler ASValue
evalCode loc values xp = do
	let lang = language xp
	let finalXp = interpolate loc values xp -- eval string

	$(logInfo) $ "EVAL RECEIVES XP: " ++ (fromString . show $ expression xp)
	$(logInfo) $ "EVAL FINAL XP: " ++ (fromString . show $ finalXp)

	simpleInterpolated <- interpolateFile lang finalXp

	-- SQL REGEX DELETE
	let interpolated = case lang of 
				SQL -> formatSqlQuery simpleInterpolated ((expression xp), rng, rangeVals)
					where 
						rng 			= P.head $ R.getExcelMatches (expression xp)
						rangeVals 		= replaceSubstrings expandedLists matches
						expandedLists 	= R.excelRangesToLists SQL rng
						matches 		= map (\(a, b) -> (R.toExcel a, R.showFilteredValue SQL a b)) (M.toList values)
				otherwise -> simpleInterpolated




	writeExecFile lang interpolated

	$(logInfo) $ "EVAL EXECUTING: " ++ (fromString $ show interpolated)
	result <- runFile lang
	$(logInfo) $ "EVAL RETURNS: " ++ (fromString result)
	return $ parseValue lang result


evalRef :: ASLocation -> Map ASLocation ASValue -> ASExpression ->  Handler ASValue
evalRef loc dict (Reference l (a, b)) = do
  $(logInfo) $ (fromString $ "evalref: "++ show dict ++ "select " ++ show (a, b))
  return $ row L.!! a
    where
      ValueL row = lst L.!! b
      ValueL lst = dict M.! l

-----------------------------------------------------------------------------------------------------------------------
-- File Manipulation

writeExecFile :: ASLanguage -> String -> Handler ()
writeExecFile lang contents = liftIO $ writeFile ((getRunFile lang) :: System.IO.FilePath) contents

-----------------------------------------------------------------------------------------------------------------------
-- Evaluation in progress

runFile :: ASLanguage -> Handler String
runFile lang = do
	let terminalCmd = addCompileCmd lang $ formatRunArgs lang (getRunnerCmd lang) (getRunFile lang) (getRunnerArgs lang)
	res <- eval terminalCmd
	$(logInfo) $ "EVAL CMD returns: " ++ (fromString res)
	return res

eval :: String -> Handler String
eval s = do 
	$(logInfo) $ "EVAL CMD: " ++ (fromString s)
	liftIO $ do
		(stdIn,stdOut,stdErr,hProcess) <- runInteractiveCommand s
		sOutput <- System.IO.hGetContents stdOut
		sErr <- System.IO.hGetContents stdErr
		foldr seq (waitForProcess hProcess) sOutput
		foldr seq (waitForProcess hProcess) sErr
		return $ case sOutput of 
			"" -> sErr
			otherwise -> sOutput













