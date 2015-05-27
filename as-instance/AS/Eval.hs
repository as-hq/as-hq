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
import System.IO                                       
import System.Process   

-- file interpolation -- (see Lang for definitions)

evalExpression :: Map ASLocation ASValue -> ASExpression -> Handler ASValue
evalExpression dict expr =
  case expr of
    Expression _ _ -> evalCode dict expr
    Reference _ _ -> evalRef dict expr


evalCode :: Map ASLocation ASValue -> ASExpression -> Handler ASValue
evalCode values xp = do
		$(logInfo) $ "EVAL RECEIVES XP: " ++ (fromString . show $ expression xp)
		$(logInfo) $ "EVAL FINAL XP: " ++ (fromString . show $ finalXp)
		interpolated <- interpolateFile lang finalXp
		writeExecFile lang interpolated
		$(logInfo) $ "EVAL EXECUTING: " ++ (fromString $ show interpolated)
		result <- runFile lang
		$(logInfo) $ "EVAL RETURNS: " ++ (fromString result)
		return $ parseValue lang result
	where
		lang 	= language xp
		finalXp = interpolate values xp

evalRef :: Map ASLocation ASValue -> ASExpression -> Handler ASValue
evalRef dict (Reference l (a, b)) = do
  $(logInfo) $ (fromString $ "EVALREF: "++show dict ++ "select " ++ show (a, b))
  return $ row L.!! a
    where
      ValueL row = lst L.!! b
      ValueL lst = dict M.! l

-- file manipulation ------

writeExecFile :: ASLanguage -> String -> Handler ()
writeExecFile lang contents = liftIO $ writeFile ((getRunFile lang) :: System.IO.FilePath) contents

-- evaluation in process ------

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


-- Expression ASLanguage String












