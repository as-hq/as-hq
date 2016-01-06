{-# LANGUAGE TemplateHaskell #-}
module AS.TH where

-- this module exists because you cannot define TH macros
-- in the same file as they're used, so might as well group them.

import Prelude 
import AS.Types.Network
import qualified AS.Config.Internal as I

import Control.Lens
import Language.Haskell.TH

-------------------------------------------------------------------------------------------------------------------------
-- error with locations

err :: Q Exp 
err = locatedError =<< location

locatedError :: Loc -> Q Exp
locatedError loc = [|(\msg -> error ("Error at " ++ $(litE $ stringL prefix) ++ msg))|]
  where
    prefix = formattedLoc ++ " ----> "
    formattedLoc = concat [file, ":", show line, ":", show col]
    file = loc_filename loc
    (line, col) = loc_start loc

-------------------------------------------------------------------------------------------------------------------------
-- compile-time settings 

getPrintSetting :: Q Exp
getPrintSetting = do
  settings <- runIO I.getSettings
  return . ConE . mkName . show $ settings^.shouldPrint