module AS.Eval.Py where 

import AS.Types
import AS.TypesHelper
import AS.Parsing
import Import
import qualified Data.Map
import qualified Data.Text.Lazy (replace)
import Control.Applicative                                   
import System.IO                                             
import System.Process   

py_eval_path = "/home/hal/code/alphasheets-progress/as-test/as-py-eval/"
py_run_path = py_eval_path ++ "run/"
py_eval_file = py_eval_path ++ "eval.py"

-- use this method
evalPy :: Map ASLocation ASValue -> ASExpression -> Handler ASValue
evalPy dict expr = do
	let matches = map (\(a,b) -> (toExcel a, show b)) (Data.Map.toList dict)
	scrubbed <- scrubCmd $ replaceSubstrings (expression expr) matches
	let filepath = py_run_path ++ scrubbed
	-- contents <- readFile filepath
	-- py_initialize
	-- mapM_ (\x -> pyImport x) (snd scrubbed) 
	-- obj <- pyRun_String ("exe("++filepath++")") (Py_file_input py_eval_file) (map (\(_,b,c)->(b,c)) matches)
	-- return $ haskToASValue . fromPyObject $ obj
	(inn, out, err, idd) <- liftIO $ runInteractiveCommand $ "python "++filepath   
	mapM_ (liftIO . ((flip hSetBinaryMode) False)) [inn, out]             
	liftIO $ hSetBuffering inn LineBuffering                          
	liftIO $ hSetBuffering out NoBuffering                            
	parsedIntro <- liftIO $ parseUntilPrompt out                      
	return $ ValueS . unlines $ parsedIntro

parseUntilPrompt :: Handle -> IO [String]                    
parseUntilPrompt out = do                                    
	latest <- System.IO.hGetLine out                                     
	if latest == ""                                            
		then return []                                           
		else (:) <$> return latest <*> parseUntilPrompt out


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
	validFuncs <- runDB $ selectList [] []
	let vf = [func | (Entity funcId func) <- validFuncs]
	let edited = replaceAliases cmd vf
	contents <- Import.readFile $ py_eval_path ++ "template.py"
	let contents' = (unlines (map (\x -> "import "++x++"\n") (snd edited))) ++ contents ++ (fst edited) ++ "\n"
	Import.writeFile (py_eval_path++"run/temp.py") contents'
	return "temp.py"

-- takes (1) cmd string, (2) tuples [(alias, identifier, import)]
-- return tuple (cmd', [import])
replaceAliases :: String -> [ASFunc] -> (String, [String])
replaceAliases cmd [] = (cmd, [])
replaceAliases cmd matches = 
	(replaceSubstrings cmd (map (\f->(unpack (aSFuncAlias f), unpack (aSFuncApply f))) presentStubs), 
	map (\f-> unpack (aSFuncImport f)) presentStubs)
		where 
			presentStubs = filter (\x -> isInfixOf (unpack (aSFuncAlias x)) cmd) matches
