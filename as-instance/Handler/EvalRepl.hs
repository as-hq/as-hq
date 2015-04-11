module Handler.EvalRepl where

import Import
import qualified CPython as py
import qualified Data.ByteString (findSubstrings)
import Data.ByteString.Char8

postEvalReplR :: String -> Handler Value
postEvalReplR cmd = do
	filepath <- matchAlias cmd
	return $ evalPy filepath

-- take a command string, match & replace aliases, insert into template.py
-- returns filepath for evaluated py file (template'.py)
matchAlias :: String -> String
matchAlias "" = "No command specified."
matchAlias cmd = do
	validPyFuncs <- runDB $ selectList [] [] :: Handler [Entity PYfunc] >>= mapM
	validHsFuncs <- runDB $ selectList [] [] :: Handler [Entity HSFunc]

	let vpf = --todo

	pyCalls = 
	-- todo

getStubTuple :: Entity 

-- takes filepath of template'.py
-- execute eval.py on filepath
-- (eval.py parses excel cmds, evals, loads into env, evals template'.py)
evalPy :: String -> IO String
evalPy "" = "No file specified."
evalPy filePath = do
	py.initialize
	eval <- py.importModule "os"
	-- todo
