{-# LANGUAGE QuasiQuotes #-}
module AS.Eval where

import Import hiding (writeFile, getLine)
import qualified Prelude as P
import Control.Applicative
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T
import AS.Eval.Lang
import AS.Types hiding (str)
import AS.DB
import AS.Parsing.In
import AS.Parsing.Out
import AS.Parsing.Common
import System.IO                                       
import System.Process   
import qualified Data.Maybe as MB

import Python

import Data.Time.Clock
import Data.Text as T (unpack,pack)

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

	time <- liftIO (getCurrentTime >>= return . utctDayTime)
	$logInfo $ "start interpolate " ++ (fromString $ show time)

	let finalXp = interpolate loc values xp -- eval string

	time <- liftIO (getCurrentTime >>= return . utctDayTime)
	$logInfo $ "end interpolate " ++ (fromString $ show time)

	$(logInfo) $ "EVAL RECEIVES XP: " ++ (fromString . show $ expression xp)
	$(logInfo) $ "EVAL FINAL XP: " ++ (fromString . show $ finalXp)

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

	time <- liftIO (getCurrentTime >>= return . utctDayTime)
	$(logInfo) $ "EVAL EXECUTING: " ++ (fromString $ show time)

	result <- handleEval lang interpolated

	time <- liftIO (getCurrentTime >>= return . utctDayTime)
	$logInfo $ "EVAL FINISHED: " ++ (fromString $ show time)

	$(logInfo) $ "EVAL RETURNS: "  ++ (fromString result)

	let parsed = parseValue lang result

	time <- liftIO (getCurrentTime >>= return . utctDayTime)
	$logInfo $ "done parsing " ++ (fromString $ show parsed)

	return parsed

evalExcel :: ASExpression -> Handler ASExpression
evalExcel xp = do
	$(logInfo) $ "EXCEL RECEIVES XP: " ++ (fromString . show $ expression xp)
	let newXp = "evalExcel(\'"++(expression xp)++"\')"

	interpolated <- interpolateFile Excel newXp

	time <- liftIO (getCurrentTime >>= return . utctDayTime)
	$logInfo $ "done interpolating "  ++ (fromString $ show interpolated)

	resultInit <- liftIO $ pyfiString interpolated

	time <- liftIO (getCurrentTime >>= return . utctDayTime)
	$logInfo $ "finished runfile eval " ++ (fromString $ show time)

	let result' = T.unpack (T.strip (T.pack resultInit)) -- no start/end whitespace
	let result = case (L.head result') of
		'\'' -> L.init (L.tail result')
		'\"' -> L.init (L.tail result')
		otherwise -> result'
	$(logInfo) $ "EXCEL RETURNS: " ++ (fromString result)
	return $ Expression result Excel



evalCodeRepl :: ASExpression -> Handler ASValue
evalCodeRepl xp = do
	let lang = language xp
	finalXp <- interpolateFileRepl lang (expression xp)
	writeReplFile lang finalXp
	result <- runReplFile lang
	replRecord <- getReplRecord lang
	$(logInfo) $ (fromString $ "EVAL REPL returns value: " ++ (show result))
	writeReplRecord lang (replRecord ++ "\n" ++ (removePrintStmt lang (expression xp)))
	return $ case lang of 
		Python -> parseValue lang result
		otherwise -> ValueS "NOT_IMPLEMENTED"

--evalRef :: ASLocation -> Map ASLocation ASValue -> ASExpression ->  Handler ASValue
--evalRef loc dict (Reference l (a, b)) = do
--  $(logInfo) $ (fromString $ "evalref: "++ show dict ++ "select " ++ show (a, b))
--  return $ row L.!! a
--    where
--      ValueL row = lst L.!! b
--      ValueL lst = dict M.! l

{-
evalRef :: ASLocation -> Map ASLocation ASValue -> ASExpression ->  Handler ASValue
evalRef loc dict (Reference l (a, b)) = do
	$(logInfo) $ (fromString $ "evalref: "++ show dict ++ "select " ++ show (a, b))
<<<<<<< HEAD
	let (ValueL lst) = dict M.! l
	let x = lst L.!! b
	let val = case x of
		ValueL row -> (row L.!! a)
		otherwise -> x
	return val -}

evalRef :: ASLocation -> Map ASLocation ASValue -> ASExpression ->  Handler ASValue
evalRef loc dict (Reference l (a, b)) = do
  -- $(logInfo) $ (fromString $ "evalref: "++ show dict ++ "select " ++ show (a, b))
  let d = dict M.! l 
  let ret = case d of
  	(ValueL lst) -> case (lst L.!!b) of
  		(ValueL row) -> (row L.!! a)
  		otherwise -> (lst L.!!b)
  	otherwise -> dict M.! loc -- current reference
  return ret


handleEval :: ASLanguage -> String -> Handler String
handleEval lang str = case lang of 
	Python -> return =<< liftIO $ pyfiString str
	Excel  -> return =<< liftIO $ pyfiString str
	otherwise -> do
		writeExecFile lang str
		time <- liftIO (getCurrentTime >>= return . utctDayTime)
		$(logInfo) $ "done with writeexecfile " ++ (fromString $ show time)
		return =<< runFile lang



-----------------------------------------------------------------------------------------------------------------------
-- File Manipulation

writeExecFile :: ASLanguage -> String -> Handler ()
writeExecFile lang contents = liftIO $ writeFile ((getRunFile lang) :: System.IO.FilePath) contents

writeReplFile :: ASLanguage -> String -> Handler ()
writeReplFile lang contents = liftIO $ writeFile ((getRunReplFile lang) :: System.IO.FilePath) contents

writeReplRecord :: ASLanguage -> String -> Handler ()
writeReplRecord lang contents = liftIO $ writeFile ((getReplRecordFile lang) :: System.IO.FilePath) contents

clearReplRecord :: ASLanguage -> Handler ()
clearReplRecord lang = liftIO $ writeFile ((getRunReplFile lang) :: System.IO.FilePath)  ""
-----------------------------------------------------------------------------------------------------------------------
-- Evaluation in progress

runFile :: ASLanguage -> Handler String
runFile lang = do
	let terminalCmd = addCompileCmd lang $ formatRunArgs lang (getRunnerCmd lang) (getRunFile lang) (getRunnerArgs lang)
	res <- eval terminalCmd lang
	$(logInfo) $ "EVAL CMD returns: " ++ (fromString res)
	return res

runReplFile :: ASLanguage -> Handler String
runReplFile lang = do
	let terminalCmd = formatRunArgs lang (getRunnerCmdRepl lang) (getRunReplFile lang) (getRunnerArgs lang)
	res <- eval terminalCmd lang
	$(logInfo) $ "EVAL CMD returns: " ++ (fromString res)
	return res

eval :: String -> ASLanguage -> Handler String
eval s lang = do 
	$(logInfo) $ "EVAL CMD: " ++ (fromString s)
	liftIO $ do
		(_,stdOut,stdErr,hProcess) <- runInteractiveCommand s
		sOutput <- System.IO.hGetContents stdOut
		sErr <- System.IO.hGetContents stdErr
		foldr seq (waitForProcess hProcess) sOutput
		foldr seq (waitForProcess hProcess) sErr
		return $ readOutput lang sOutput sErr
		return sOutput

readOutput :: ASLanguage -> String -> String -> String
readOutput lang res err = case err of 
	"" -> res
	otherwise -> case lang of 
		Python -> case res of 
			"" -> res
			otherwise -> res
		OCaml -> err
		otherwise -> err


-- until we fix matplotlibrc's bullshit
--readOutput :: ASLanguage -> String -> String -> String
--readOutput lang res err = case err of 
--	"" -> res
--	otherwise -> case lang of 
--		Python -> case res of 
--			"" -> "{\'error\':\'" ++ err ++ "\', \'err_type\':\'Evaluation\', \'position\':0, \'file\':\'temp.py\'}"
--			otherwise -> res
--		OCaml -> err
--		otherwise -> err


------------------- PYfi python evaluation --------------------

evalString :: ASLanguage -> String -> IO ASValue 
evalString lang evalStr = return . parseValue lang =<< pyfiString evalStr

pyfiString :: String -> IO String
pyfiString evalStr = defVV (evalStr ++ pyString) ("Hello" :: String)

pyString :: String
pyString = [str|
def export(x=1):
	return repr(result)
|]