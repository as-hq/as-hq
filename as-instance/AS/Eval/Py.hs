module AS.Eval.Py where 

import AS.DB
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

py_eval_path = "/home/hal/code/branches/alphasheets/as-images/as-instance/as-py-eval/"
py_libs_path = "/home/hal/code/branches/alphasheets/as-images/as-libs/py/"
py_run_path = py_eval_path ++ "run/"
py_template_file = "template.py"
py_eval_file = "eval.py"
py_run_file = "temp.py"

evalExpression :: Map ASLocation ASValue -> ASExpression -> Handler ASValue
evalExpression dict expr =
  case expr of
    Expression _ -> evalPy dict expr
    Reference _ _ -> evalRef dict expr

evalRef :: Map ASLocation ASValue -> ASExpression -> Handler ASValue
evalRef dict (Reference l (a, b)) = do
  $(logInfo) $ (fromString $ "EVALREF: "++show dict ++ "select " ++ show (a, b))
  return $ row L.!! a
    where
      ValueL row = lst L.!! b
      ValueL lst = dict M.! l

-- use this method
evalPy :: Map ASLocation ASValue -> ASExpression -> Handler ASValue
evalPy dict expr = do
  $(logInfo) $ "EVALPY with EXPR: " ++ (fromString $ show expr)
  let matches = map (\(a,b) -> (toExcel a, showFilteredValue a b)) (M.toList dict)

  $(logInfo) $ "EVALPY with MATCHES: " ++ (fromString $ show dict)

  let expr' = excelRangesToIterables $ expression expr
  $(logInfo) $ "EVALPY with EXPANDED MATHCES: " ++ (fromString $ show expr')
  scrubCmd $ replaceSubstrings expr' matches

  let runfile = py_run_path ++ py_run_file
  let execCmd = "python "++py_eval_path++py_eval_file++" "++runfile :: String

  $(logInfo) $ "EVALPY with command: " ++ (fromString $ show execCmd)

  result <- liftIO $ eval execCmd
  $(logInfo) $ "EVALPY returns result: " ++ (fromString $ result)

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
scrubCmd :: String -> Handler ()
scrubCmd cmd = do
  $(logInfo) $ "EVALPY with init cmd: " ++ (fromString $ show cmd)

  functions <- getFuncs
  let (cmd', imports) = replaceAliases cmd functions
      editedCmd = replaceSubstrings cmd' [(";", "\n")]
      importCmds = unlines . map (\(name,command) -> 
        if command == "execfile" 
          then command ++ "(\"" ++ py_libs_path ++ name ++ ".py" ++ "\")\n" 
          else command ++ "\n"
        ) $ imports

  $(logInfo) $ "EVALPY with edited cmd: " ++ (fromString $ show (editedCmd, importCmds))

  contents <- Import.readFile $ py_eval_path ++ py_template_file
  let codeFile = importCmds ++ contents ++ "\n" ++ editedCmd

  $(logInfo) $ "EVALPY with cmd'': " ++ (fromString $ show codeFile)

  Import.writeFile (py_run_path ++ py_run_file) codeFile

-- takes (1) cmd string, (2) funcs [ASFunc]
-- return tuple (cmd', [(importName, importCommand)])
replaceAliases :: String -> [ASFunc] -> (String, [(String, String)])
replaceAliases cmd [] = (cmd, [])
replaceAliases cmd matches = 
	(replaceSubstrings cmd (map toReplacingImports presentStubs), 
	map (\f-> (unpack (aSFuncImportName f), unpack (aSFuncImportCommand f))) presentStubs)
		where 
			toReplacingImports = (\f->(unpack (aSFuncAlias f), unpack (aSFuncReplace f)))
			presentStubs = filter (\x -> isInfixOf (unpack (aSFuncAlias x)) cmd) matches
