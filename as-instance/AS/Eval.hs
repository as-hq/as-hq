module AS.Eval where

import Import hiding (writeFile)
import qualified Prelude as P
import Control.Applicative
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T
import AS.Eval.Lang
import AS.Types
import AS.Parsing
import AS.DB
import System.IO                                       
import System.Process   


evalExpression :: Map ASLocation ASValue -> ASExpression -> Handler ASValue
evalExpression dict expr =
  case expr of
    Expression _ _ -> evalCode dict expr
    Reference _ _ -> evalRef dict expr

-- Expression ASLanguage String

showFilteredValue :: ASLanguage -> ASLocation -> ASValue -> String
showFilteredValue lang (Index i) (ValueL l) = showFilteredValue lang (Index i) (headOrNull l)
  where
    headOrNull [] = ValueNaN ()
    headOrNull (x:xs) = x
showFilteredValue lang _ a = showValue lang a

interpolate :: Map ASLocation ASValue -> ASExpression -> String
interpolate values xp = execCmd
	where
		execCmd			= replaceSubstrings expandedLists matches
		expandedLists 	= excelRangesToLists lang $ expression xp
		matches 		= map (\(a, b) -> (toExcel a, showFilteredValue lang a b)) (M.toList values)
		lang 			= language xp

insertPrintCmd :: ASLanguage -> (String, String) -> String
insertPrintCmd lang (s, lst) = s ++ process lst 
	where
		process l 	= case lang of 
			R -> "cat(toJSON(" ++ l ++ "))" 
			Python -> "print(repr(" ++ l ++ "))"
			OCaml -> "print_string(Std.dump(" ++ l ++ "))"

splitLastCmd :: ASLanguage -> String -> (String, String)
splitLastCmd lang cmd = 
	let 
		lines = T.splitOn (pack "\n") (pack cmd)
		lastLine = T.splitOn (pack $ getInlineDelim lang) (P.last lines)
		top = (concat $ map unpack (P.init lines)) ++ (concat (map unpack $ P.init lastLine)) ++ (getLineDelim lang) ++ "\n"
		lastStmt = unpack $ P.last lastLine
	in (top, lastStmt)

interpolateFile :: ASLanguage -> String -> Handler String
interpolateFile lang execCmd = do
	functions <- getFuncs lang
	let (cleanCmd, imports) = replaceAliases execCmd functions
	let editedCmd = insertPrintCmd lang $ splitLastCmd lang cleanCmd
	let importCmds = unlines . map (importFile lang) $ imports
	template <- getTemplate lang
	return $ intercalate "\n" [importCmds, template, editedCmd]

writeExecFile :: ASLanguage -> String -> Handler ()
writeExecFile lang contents = liftIO $ writeFile ((getRunFile lang) :: System.IO.FilePath) contents

eval :: String -> Handler String
eval s = do 
	$(logInfo) $ "EVAL CMD: " ++ (fromString s)
	liftIO $ do
		(_,hOutput,_,hProcess) <- runInteractiveCommand s
		sOutput <- System.IO.hGetContents hOutput
		foldr seq (waitForProcess hProcess) sOutput
		return sOutput

a <++> b = (++) <$> a <*> b

runFile :: ASLanguage -> Handler String
runFile lang = do
	let terminalCmd = addCompileCmd lang $ formatRunArgs lang (getRunnerCmd lang) (getRunFile lang) (getRunnerArgs lang)
	res <- eval terminalCmd
	$(logInfo) $ "EVAL CMD returns: " ++ (fromString res)
	return res

evalRef :: Map ASLocation ASValue -> ASExpression -> Handler ASValue
evalRef dict (Reference l (a, b)) = do
  $(logInfo) $ (fromString $ "EVALREF: "++show dict ++ "select " ++ show (a, b))
  return $ row L.!! a
    where
      ValueL row = lst L.!! b
      ValueL lst = dict M.! l

evalCode :: Map ASLocation ASValue -> ASExpression -> Handler ASValue
evalCode values xp = do
		interpolated <- interpolateFile lang finalXp
		writeExecFile lang interpolated
		result <- runFile lang
		$(logInfo) $ "EVAL RETURNS: " ++ (fromString result)
		return $ parseValue lang result
	where
		lang 	= language xp
		finalXp = interpolate values xp
