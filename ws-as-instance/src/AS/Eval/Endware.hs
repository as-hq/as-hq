module AS.Eval.Endware where

import Prelude

import AS.Types

-- here, we apply a stack of endwares.

-- examples: 
-- green(x) in python -> produces styled value with string in output -> string parsed to Color tag
-- Bloomberg(x) in java -> produces json with stream specs -> converted to Stream tag, kickoff daemon

evalEndware :: ASCell -> IO ASCell
evalEndware cell = 
    tagStyledCell cell >>= 
        tagStreamedCell >>=
            return

----------------------------------------------------------------------------------------------------------------------------------------------
-- | endwares

tagStyledCell :: ASCell -> IO ASCell 
tagStyledCell c@(Cell l e v t) = return c -- TODO

tagStreamedCell :: ASCell -> IO ASCell 
tagStreamedCell c@(Cell l e v t) = return c -- TODO