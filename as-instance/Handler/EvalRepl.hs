module Handler.EvalRepl where

import Import
import qualified CPython as py
import Data.Text.Lazy

postEvalReplR :: String -> Handler Value
postEvalReplR cmd = do
	filepath <- scrubCmd cmd
	return $ evalPy filepath

-- take a command string, match & replace aliases, insert into template.py
-- returns filepath for scrubbed py file (template'.py)
scrubCmd :: String -> String
scrubCmd "" = "No command specified."
scrubCmd cmd = do
	validFuncs <- runDB $ selectList [] [Entity ASfunc]
	let vf = --todo

	let edited = replaceAliases (pack cmd) vf

	-- todo

-- takes (1) cmd string, (2) tuple of (alias, identifier, import)
-- return tuple (cmd', [import])
replaceAliases :: Text -> [(Text, Text, Text)] -> (Text, [Text])
replaceAliases cmd [] = (cmd, [])
replaceAliases cmd matches = (replaceAliases' cmd . (\(a,b,_)->(a,b)) presentStubs, 
		(\(_,_,c)->c)) presentStubs)
	-- filter present func usages
	where presentStubs = (\(a,_,_)-> isInfixOf a cmd) matches
		  -- actual work
		  replaceAliases' m [] = m
		  replaceAliases' m (x:xs) = replaceAliases' scrubbed xs
		  	where scrubbed = replace (fst x) m (snd x)


-- takes filepath of template'.py
-- execute eval.py on filepath
-- (eval.py parses excel cmds, evals, loads into env, evals template'.py)
evalPy :: String -> IO String
evalPy "" = "No file specified."
evalPy filePath = do
	py.initialize
	eval <- py.importModule "os"
	-- todo
