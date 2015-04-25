module AS.Eval.Py where 

import AS.Types
import AS.TypesHelper
import AS.Parsing
import Import
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text.Lazy (replace)
import Control.Applicative                                   
import System.IO                                             
import System.Process   

py_eval_path = "/Users/zeigjeder/Development/alphasheets/alpha-sheets/as-instance/as-py-eval/"
py_run_path = py_eval_path ++ "run/"
py_eval_file = py_eval_path ++ "eval.py"

evalExpression :: Map ASLocation ASValue -> ASExpression -> Handler ASValue
evalExpression dict expr =
  case expr of
    Expression _ -> evalPy dict expr
    Reference _ _ -> evalRef dict expr

evalRef :: Map ASLocation ASValue -> ASExpression -> Handler ASValue
evalRef dict (Reference l (a, b)) = return $ row L.!! a
  where
    ValueL row = lst L.!! b
    ValueL lst = dict M.! l

-- use this method
evalPy :: Map ASLocation ASValue -> ASExpression -> Handler ASValue
evalPy dict expr = do
	let matches = map (\(a,b) -> (toExcel a, showValue b)) (M.toList dict)

	$(logInfo) $ "EVALPY with MATCHES: " ++ (fromString $ show dict)

	let expr' = excelRangesToLists $ expression expr
	scrubbed <- scrubCmd $ replaceSubstrings expr' matches

	let filepath = py_run_path ++ scrubbed
	let execCmd = "python "++py_eval_file++" "++filepath 

	$(logInfo) $ "EVALPY with command: " ++ (fromString $ show execCmd)
	result <- liftIO $ eval execCmd

	return $ parseValue (filter (/= '\n') result)

eval :: String -> IO String
eval s = do 
	(_,hOutput,_,hProcess) <- runInteractiveCommand s
	sOutput <- System.IO.hGetContents hOutput
	foldr seq (waitForProcess hProcess) sOutput
	return sOutput

-- convenience method for string-only cmd, i.e. in evalRepl route
-- evalPy :: String -> IO a
-- evalPy "" = "No command specified."
-- evalPy cmd = do
-- 	scrubbed <- scrubCmd cmd
-- 	let filepath = py_run_path ++ (fst scrubbed)
-- 	contents <- readFile filepath
-- 	py_initialize
-- 	mapM_ (\x -> pyImport x) (snd scrubbed) 
-- 	obj <- pyRun_String ("exe("++filepath++")") (Py_file_input py_eval_file) []
-- 	return $ fromPyObject obj

-- take a command string, match & replace aliases, insert into template.py
-- returns filename for scrubbed py file (temp.py) & list of improts
scrubCmd :: String -> Handler String
scrubCmd "" = return "No command specified."
scrubCmd cmd = do
	$(logInfo) $ "EVALPY with init cmd: " ++ (fromString $ show cmd)
	validFuncs <- runDB $ selectList [] []
	let vf = [func | (Entity funcId func) <- validFuncs]
	let edited = replaceAliases cmd vf
	$(logInfo) $ "EVALPY with edited cmd: " ++ (fromString $ show edited)
	contents <- Import.readFile $ py_eval_path ++ "template.py"
	let contents' = (unlines (map (\(apply, path) -> apply++"(\""++path++"\")\n") (snd edited))) ++ contents
	let contents'' = contents' ++ "\n" ++ (fst edited)
	$(logInfo) $ "EVALPY with cmd'': " ++ (fromString $ show contents'')
	Import.writeFile (py_eval_path++"run/temp.py") contents''
	return "temp.py"

-- takes (1) cmd string, (2) tuples [(alias, apply, path)]
-- return tuple (cmd', [(applicative command, func path)])
replaceAliases :: String -> [ASFunc] -> (String, [(String, String)])
replaceAliases cmd [] = (cmd, [])
replaceAliases cmd matches = 
	(replaceSubstrings cmd (map toReplacingImports presentStubs), 
	map (\f-> (unpack (aSFuncApply f), unpack (aSFuncPath f))) presentStubs)
		where 
			toReplacingImports = (\f->(unpack (aSFuncAlias f), unpack (aSFuncReplace f)))
			presentStubs = filter (\x -> isInfixOf (unpack (aSFuncAlias x)) cmd) matches
