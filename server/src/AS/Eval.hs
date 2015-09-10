{-# LANGUAGE QuasiQuotes #-}
module AS.Eval where

import Prelude
import System.IO           
import System.Process  
import Python
import Control.Applicative
 
import qualified Data.Maybe as MB
import Data.Time.Clock
import Data.Text as T (unpack,pack)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T

import AS.Eval.Lang
import AS.Types hiding (str)
import AS.DB hiding (expression)
import AS.Parsing.In
import AS.Parsing.Out
import AS.Parsing.Common
import AS.Util
import AS.Config.Settings as S

-----------------------------------------------------------------------------------------------------------------------
-- Exposed functions

evalExpression :: ASLocation -> M.Map ASLocation ASValue -> ASExpression -> IO ASValue
evalExpression loc dict expr =
  case expr of
    Expression _ _ -> evalCode (locSheetId loc) dict expr  
    Reference _ _ -> evalRef loc dict expr  

-- | Returns the new Python expression and a boolean for whether the expression is volatile or not
evalExcel :: ASExpression -> IO (Either ASValue (ASExpression,Bool)) 
evalExcel xp = do
	--printTimed $ "eval inital excel xp: " ++ (show . expression $ xp)
	let newXp = "evalExcel('"++(expression xp)++"')"
	interpolated <- interpolateFile Excel newXp
	--putStrLn $ "EXCEL evaluating file: \n" ++ interpolated
	resultInit <- pyfiString interpolated
	--putStrLn $ "EXCEL RESULT INIT: " ++ (show resultInit)
	{-
		let result' = T.unpack (T.strip (T.pack resultInit)) -- no start/end whitespace
		let result = case (L.head result') of
			'\'' -> L.init (L.tail result')
			'\"' -> L.init (L.tail result')
			otherwise -> result'
	-}
	let result = parseValue Excel resultInit
	return $ case result of 
		(ValueL [ValueS s, ValueB b]) -> Right (Expression s Excel, b)
		e@(ValueError _ _ _ _) -> Left e

-----------------------------------------------------------------------------------------------------------------------
-- File Interpolation (see Lang for details)

evalCode :: ASSheetId -> M.Map ASLocation ASValue -> ASExpression -> IO ASValue
evalCode sheetid values xp = do
	printTimed "Starting eval code"
	let lang = language xp
	let finalXp = interpolate sheetid values xp 
	--printTimed $ "Final eval xp: " ++ (show finalXp)
	simpleInterpolated <- interpolateFile lang finalXp
	interpolated <- case lang of
		SQL -> interpolateFile SQL ("setGlobals("++(show context) ++")\n" ++ newExp)
			where
				exLocs = getMatchesWithContext (expression xp) excelMatch
				matchLocs = map (exLocToASLocation sheetid) (snd exLocs)
				context = map (lookupString SQL values) matchLocs
				st = ["dataset"++(show i) | i<-[0..((L.length matchLocs)-1)]]
				newExp = replaceMatches exLocs (\el -> (L.!!) st (MB.fromJust (L.findIndex (el==) (snd exLocs)))) (expression xp)
		otherwise -> (return simpleInterpolated)
	printTimed "starting eval"
	result <- doEval lang interpolated
	--printTimed $ "finished eval " ++ (show result)
	let parsed = parseValue lang result
	--printTimed $ "eval result parsed " ++ (show parsed)
	return parsed


evalCodeRepl :: ASExpression -> IO ASValue
evalCodeRepl xp = do
	let lang = language xp
	finalXp <- interpolateFileRepl lang (expression xp)
	writeReplFile lang finalXp
	result <- pyfiString finalXp
	replRecord <- getReplRecord lang
	let parsed = parseValue lang result
	case parsed of 
		(ValueError _ _ _ _) -> return ()
		otherwise -> writeReplRecord lang (replRecord ++ "\n" ++ (removePrintStmt lang (expression xp)))
	return parsed

evalRef :: ASLocation -> M.Map ASLocation ASValue -> ASExpression ->  IO ASValue
evalRef loc dict (Reference l (a, b)) = do
  let d = dict M.! l 
  let ret = case d of
  	(ValueL lst) -> case (lst L.!!b) of
  		(ValueL row) -> (row L.!! a)
  		otherwise -> (lst L.!!b)
  	otherwise -> dict M.! loc -- current reference
  return ret

-- TODO architectural decision of file-based vs kernel-based eval
doEval :: ASLanguage -> String -> IO String
doEval lang str = case lang of 
	Python -> pyfiString str
	Excel  -> writeExecFile lang str >> pyfiString str
	SQL	   -> if S.isDebug  -- if isDebug, write the python exec file
		then do
			writeExecFile lang str
			--printTimed "did eval!"
			return =<< pyfiString str
		else pyfiString str
	otherwise -> do 		-- all other languages follow file-based eval for now
		writeExecFile lang str
		time <- getCurrentTime >>= return . utctDayTime
		return =<< runFile lang

-----------------------------------------------------------------------------------------------------------------------
-- File Manipulation

writeExecFile :: ASLanguage -> String -> IO ()
writeExecFile lang contents = getRunFile lang >>= \f -> writeFile (f :: System.IO.FilePath) contents

writeReplFile :: ASLanguage -> String -> IO ()
writeReplFile lang contents = getRunReplFile lang >>= \f -> writeFile (f :: System.IO.FilePath) contents

writeReplRecord :: ASLanguage -> String -> IO ()
writeReplRecord lang contents = getReplRecordFile lang >>= \f -> writeFile (f :: System.IO.FilePath) contents

clearReplRecord :: ASLanguage -> IO ()
clearReplRecord lang = getReplRecordFile lang >>= \f -> writeFile (f :: System.IO.FilePath)  ""

-----------------------------------------------------------------------------------------------------------------------
-- Evaluation in progress

runFile :: ASLanguage -> IO String
runFile lang = do
	args <- getRunnerArgs lang
	file <- getRunFile lang
	terminalCmd <- addCompileCmd lang $ formatRunArgs lang (getRunnerCmd lang) file args
	res <- eval terminalCmd lang
	return res

runReplFile :: ASLanguage -> IO String
runReplFile lang = do
	args <- getRunnerArgs lang
	file <- getRunReplFile lang
	let terminalCmd = formatRunArgs lang (getRunnerCmdRepl lang) file args
	res <- eval terminalCmd lang
	return res

eval :: String -> ASLanguage -> IO String
eval s lang = do 
	(_,stdOut,stdErr,hProcess) <- runInteractiveCommand s
	sOutput <- System.IO.hGetContents stdOut
	sErr <- System.IO.hGetContents stdErr
	foldr seq (waitForProcess hProcess) sOutput
	foldr seq (waitForProcess hProcess) sErr
	return $ readOutput lang sOutput sErr

readOutput :: ASLanguage -> String -> String -> String
readOutput lang res err = case err of 
	"" -> res
	otherwise -> case lang of 
		Python -> case res of 
			"" -> err
			otherwise -> res
		OCaml -> err
		otherwise -> err

------------------- PYfi python evaluation --------------------

evalString :: ASLanguage -> String -> IO ASValue 
evalString lang evalStr = return . parseValue lang =<< pyfiString evalStr

pyfiString :: String -> IO String
pyfiString evalStr = do 
	--_ <- putStrLn $ "IN PYFI"
	defVV (evalStr ++ pyString) ("Hello" :: String)

pyString :: String
pyString = [str|
def export(x=1):
	return repr(result)
|]