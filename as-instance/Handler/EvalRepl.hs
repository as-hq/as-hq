module Handler.EvalRepl where

import Import
--import qualified CPython as py
import qualified Data.Text.Lazy (replace)

postEvalReplR :: String -> Handler Value
postEvalReplR cmd = do
	filepath <- scrubCmd cmd
	return filepath
	--return $ evalPy filepath

-- take a command string, match & replace aliases, insert into template.py
-- returns filepath for scrubbed py file (template'.py)
scrubCmd :: String -> String
scrubCmd "" = "No command specified."
scrubCmd cmd = do
	validFuncs <- runDB $ selectList [] []
	let vf = map (\func -> (ASFuncAlias $ entityVal func, ASFuncApply $ entityVal func, ASFuncImport $ entityVal func))
	let edited = replaceAliases (pack cmd) vf
	return $ show $ fst edited

	-- todo

-- takes (1) cmd string, (2) tuple of (alias, identifier, import)
-- return tuple (cmd', [import])
replaceAliases :: Text -> [(Text, Text, Text)] -> (Text, [Text])
replaceAliases cmd [] = (cmd, [])
replaceAliases cmd matches = (replaceAliases' cmd (map ((\(a,b,_)->(a,b)) presentStubs)), 
		map (\(_,_,c)->c) presentStubs)
		where 
			presentStubs = filter (\(a,_,_)-> isInfixOf a cmd) matches
			replaceAliases' m [] = m
			replaceAliases' m (x:xs) = replaceAliases' scrubbed xs
				where scrubbed = Data.Text.Lazy.replace (fst x) m (snd x)


-- takes filepath of template'.py
-- execute eval.py on filepath
-- (eval.py parses excel cmds, evals, loads into env, evals template'.py)
evalPy :: String -> IO String
evalPy "" = "No file specified."
evalPy filePath = do
	py.initialize
	eval <- py.importModule "os"
	-- todo
