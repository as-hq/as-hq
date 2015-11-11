{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module AS.Parsing.Objects where

import Prelude
import Data.List (elemIndex)
import Data.Maybe
import Data.Char as C
import qualified Data.Text as T
import qualified Data.List as L
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many)

import AS.Types.Core
import AS.Parsing.Common

--serialize :: String -> 'NoValue 
--serialize str = NoValue

data FuckPrim = Fuck1' | Fuck2'

data Fuck :: FuckPrim -> * where
  Fuck1 :: {fuck :: String } -> Fuck Fuck1'

deserialize :: Fuck a -> String
deserialize (Fuck1 s) = s

--deserialize' :: Fuck -> String
--deserialize' (Fuck1 s) = s

--jsonDeserialize :: ASLanguage -> String -> String -> String
--jsonDeserialize lang objType jsonRep =
--  let dlm = getBlockDelim lang
--  in case lang of
--    R       -> objType ++ "$(" ++ jsonRep ++ ")" ++ dlm
--    Python  -> objType ++ ".deserialize(" ++ jsonRep ++ ")" ++ dlm
--    OCaml   -> "Serialization# " ++ objType ++ " " ++ jsonRep ++ dlm
--    SQL     -> objType ++ ".deserialize(" ++ jsonRep ++ ")" ++ dlm