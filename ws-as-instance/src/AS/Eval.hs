{-# LANGUAGE QuasiQuotes #-}
module AS.Eval where

import Prelude
import qualified Prelude as P
import Control.Applicative
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T
import AS.Eval.Lang
import AS.Types hiding (str)
import AS.DB hiding (expression)
import AS.Parsing.In
import AS.Parsing.Out
import AS.Parsing.Common
import System.IO                        
import System.Process   
import qualified Data.Maybe as MB

import Python

import Data.Time.Clock
import Data.Text as T (unpack,pack)

printTime str = do 
  time <- (getCurrentTime >>= return . utctDayTime)
  putStrLn $ str ++ " " ++ (show time)

-----------------------------------------------------------------------------------------------------------------------
-- File Interpolation (see Lang for details)

evalExpression :: ASLocation -> M.Map ASLocation ASValue -> ASExpression -> IO ASValue
evalExpression loc dict expr =
  case expr of
    Expression _ _ -> evalCode loc dict expr  
    Reference _ _ -> evalRef loc dict expr  

evalCode :: ASLocation -> M.Map ASLocation ASValue -> ASExpression -> IO ASValue
evalCode loc values xp = do
	printTime "Starting eval code"
	let lang = language xp
	let finalXp = interpolate loc values xp -- eval string
	simpleInterpolated <- interpolateFile lang finalXp

	interpolated <- case lang of
		SQL -> interpolateFile SQL ("setGlobals("++(show context) ++")\n" ++ newExp)
			where
				exLocs = getMatchesWithContext (expression xp) excelMatch
				matchLocs = map (exLocToASLocation loc) (snd exLocs)
				context = map (lookupString SQL values) matchLocs
				st = ["dataset"++(show i) | i<-[0..((L.length matchLocs)-1)]]
				newExp = replaceMatches exLocs (\el -> (L.!!) st (MB.fromJust (L.findIndex (el==) (snd exLocs)))) (expression xp)
		otherwise -> (return simpleInterpolated)


	printTime "starting eval"
	result <- handleEval lang interpolated
	printTime "finished eval"
	let parsed = parseValue lang result
	printTime "eval result parsed"
	return parsed

evalExcel :: ASExpression -> IO ASExpression
evalExcel xp = do
	let newXp = "evalExcel(\'"++(expression xp)++"\')"

	interpolated <- interpolateFile Excel newXp

	time <- (getCurrentTime >>= return . utctDayTime)

	resultInit <- pyfiString interpolated

	time <- (getCurrentTime >>= return . utctDayTime)

	let result' = T.unpack (T.strip (T.pack resultInit)) -- no start/end whitespace
	let result = case (L.head result') of
		'\'' -> L.init (L.tail result')
		'\"' -> L.init (L.tail result')
		otherwise -> result'
	return $ Expression result Excel



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

--evalRef :: ASLocation -> Map ASLocation ASValue -> ASExpression ->  IO ASValue
--evalRef loc dict (Reference l (a, b)) = do
--  $(logInfo) $ (fromString $ "evalref: "++ show dict ++ "select " ++ show (a, b))
--  return $ row L.!! a
--    where
--      ValueL row = lst L.!! b
--      ValueL lst = dict M.! l

{-
evalRef :: ASLocation -> Map ASLocation ASValue -> ASExpression ->  IO ASValue
evalRef loc dict (Reference l (a, b)) = do
	$(logInfo) $ (fromString $ "evalref: "++ show dict ++ "select " ++ show (a, b))
<<<<<<< HEAD
	let (ValueL lst) = dict M.! l
	let x = lst L.!! b
	let val = case x of
		ValueL row -> (row L.!! a)
		otherwise -> x
	return val -}

evalRef :: ASLocation -> M.Map ASLocation ASValue -> ASExpression ->  IO ASValue
evalRef loc dict (Reference l (a, b)) = do
  -- $(logInfo) $ (fromString $ "evalref: "++ show dict ++ "select " ++ show (a, b))
  let d = dict M.! l 
  let ret = case d of
  	(ValueL lst) -> case (lst L.!!b) of
  		(ValueL row) -> (row L.!! a)
  		otherwise -> (lst L.!!b)
  	otherwise -> dict M.! loc -- current reference
  return ret


handleEval :: ASLanguage -> String -> IO String
handleEval lang str = case lang of 
	Python -> pyfiString str
	Excel  -> pyfiString str
	SQL	   -> do
		writeExecFile lang str
		time <- getCurrentTime >>= return . utctDayTime
		return =<< pyfiString str
	otherwise -> do
		writeExecFile lang str
		time <- getCurrentTime >>= return . utctDayTime
		return =<< runFile lang



-----------------------------------------------------------------------------------------------------------------------
-- File Manipulation

writeExecFile :: ASLanguage -> String -> IO ()
writeExecFile lang contents = writeFile ((getRunFile lang) :: System.IO.FilePath) contents

writeReplFile :: ASLanguage -> String -> IO ()
writeReplFile lang contents = writeFile ((getRunReplFile lang) :: System.IO.FilePath) contents

writeReplRecord :: ASLanguage -> String -> IO ()
writeReplRecord lang contents = writeFile ((getReplRecordFile lang) :: System.IO.FilePath) contents

clearReplRecord :: ASLanguage -> IO ()
clearReplRecord lang = writeFile ((getReplRecordFile lang) :: System.IO.FilePath)  ""
-----------------------------------------------------------------------------------------------------------------------
-- Evaluation in progress

runFile :: ASLanguage -> IO String
runFile lang = do
	let terminalCmd = addCompileCmd lang $ formatRunArgs lang (getRunnerCmd lang) (getRunFile lang) (getRunnerArgs lang)
	res <- eval terminalCmd lang
	return res

runReplFile :: ASLanguage -> IO String
runReplFile lang = do
	let terminalCmd = formatRunArgs lang (getRunnerCmdRepl lang) (getRunReplFile lang) (getRunnerArgs lang)
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
	_ <- putStrLn $ "IN PYFI"
	defVV (evalStr ++ pyString) ("Hello" :: String)

pyString :: String
pyString = [str|
def export(x=1):
	return repr(result)
|]