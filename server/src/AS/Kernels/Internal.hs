module AS.Kernels.Internal where

import AS.Types.Cell

import System.Process
import System.IO


-----------------------------------------------------------------------------------------------------------------------
-- Evaluation

evalShell :: ASLanguage -> String -> IO String
evalShell lang s = do 
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

