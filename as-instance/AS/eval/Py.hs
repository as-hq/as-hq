module AS.Eval.Py where 

import AS.Types
import Foundation (runDB)
import Data.Map
import qualified Data.Text.Lazy (replace)

py_eval_path = "/home/hal/code/alphasheets-progress/as-test/as-py-eval/"
py_run_path = py_eval_path ++ "run/"
py_eval_file = py_eval_path ++ "eval.py"

-- use this method
evalPy :: Map ASLocation ASValue -> ASExpression -> IO ASValue
evalPy dict expr = do
	let matches = map (\(a,b) -> 
		(xl, varName xl, ToPyObject b)) $ toList dict
	where
		xl a = locationToExcel a
		varName a = filter (\x-> x/=':') a
	scrubbed <- scrubCmd $ replaceTexts (pack expr) (map (\(a,b,_)->(pack a,pack b)) matches)
	let filepath = py_run_path ++ $ fst scrubbed
	contents <- readFile filepath
	py_initialize
	mapM_ (\x -> pyImport x) (snd scrubbed) 
	obj <- pyRun_String ("exe("++filePath++")") (Py_file_input py_eval_file) (map (\(_,b,c)->(b,c)) matches)
	return $ ASValue $ FromPyObject obj


-- convenience method for string-only cmd, i.e. in evalRepl route
evalPy :: String -> IO a
evalPy "" = "No command specified."
evalPy cmd = do
	scrubbed <- scrubCmd cmd
	let filepath = py_run_path ++ $ fst scrubbed
	contents <- readFile filepath
	py_initialize
	mapM_ (\x -> pyImport x) (snd scrubbed) 
	obj <- pyRun_String ("exe("++filePath++")") (Py_file_input py_eval_file) []
	return $ FromPyObject obj

-- take a command string, match & replace aliases, insert into template.py
-- returns filename for scrubbed py file (temp.py) & list of improts
scrubCmd :: Text -> IO (String, [Text])
scrubCmd "" = "No command specified."
scrubCmd cmd = do
	validFuncs <- runDB $ selectList [] []
	let vf = [func | (Entity funcId func) <- validFuncs]
	let edited = replaceAliases (pack cmd) vf
	contents <- readFile py_eval_path ++ "template.py"
	writeFile (contents ++ (fst edited) ++ "\n") (py_eval_path++"run/temp.py")
	return ("temp.py", snd edited)

-- takes (1) cmd string, (2) tuples [(alias, identifier, import)]
-- return tuple (cmd', [import])
replaceAliases :: Text -> [ASFunc] -> (Text, [Text])
replaceAliases cmd [] = (cmd, [])
replaceAliases cmd matches = 
	(replaceTexts (fromStrict cmd) (map (\f->(ASFuncAlias f, ASFuncIdentifier f)) presentStubs), 
	map (\f->ASFuncImport f) presentStubs)
		where 
			presentStubs = filter (\f-> isInfixOf (ASFuncAlias f) cmd) matches

-- replace substrings
replaceTexts :: Text -> [(Text, Text)] -> Text
replaceTexts m [] = m
replaceTexts m (x:xs) = replaceTexts scrubbed xs
where scrubbed = Data.Text.Lazy.replace (fst x) m (snd x)


