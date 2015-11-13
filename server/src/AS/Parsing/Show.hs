module AS.Parsing.Show where

import Prelude
import Data.List (elemIndex)
import Data.Maybe
import Data.Char as C
import qualified Data.Text as T
import qualified Data.List as L
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many)
import qualified Data.Map as M
import qualified Data.Text.Lazy (replace)

import AS.Types.Core
import AS.Types.Excel
import AS.Parsing.Common
import AS.Util
import AS.LanguageDefs as LD

-----------------------------------------------------------------------------------------------------------------------
-- exposed

showValue :: ASLanguage -> CompositeValue -> String
showValue lang v = case v of
  NoValue            -> LD.null lang
  ValueS s           -> show s
  ValueI i           -> show i
  ValueD d           -> show d
  ValueB b           -> bool lang b
  _ -> error ("In showValue, failed to pattern match: " ++ (show v))

-----------------------------------------------------------------------------------------------------------------------
-- helpers

--list :: ASLanguage -> [String] -> String
--list lang xs = wrapL ++ start ++ (L.intercalate dlm lst) ++ end ++ wrapR
--  where
--    (start, end) = listStops lang
--    dlm = listDelimeter lang 
--    (wrapL, wrapR) = case lang of 
--      Python -> ("arr(", ")")
--      _ -> ("", "") 

--object :: ASLanguage -> ASValue -> String
--object lang (ValueObject otype olist oattrs) =
--  let 
--    dlm = LD.blockDelimiter lang
--    olist' = showValue lang (ValueL olist)
--  in foldr (++) $ case lang of
--    Python  -> ["deserialize(", (show otype), ",", olist, (show oattrs), ")", dlm]
--    _ -> error "cannot deserialize object in languages other than python"

bool :: ASLanguage -> Bool -> String
bool lang b = case lang of
  Python-> show b
  R     -> map C.toUpper $ show b
  OCaml -> (\str -> (C.toLower (head str)):(tail str)) $ show b
  SQL   -> show b
  Excel -> show b


--showRList :: ASLanguage -> [(RListKey, ASValue)] -> String
--showRList lang l = case lang of
--  R -> "list(" ++ (concat $ L.intersperse "," $ map showRPair l) ++ ")"

--showRPair :: (RListKey, ASValue) -> String
--showRPair (key, val) = case key of
--  "" -> showValue R val
--  _ -> key ++ "=" ++ (showValue R val)

--showRDataFrame :: ASLanguage -> [ASValue] -> String
--showRDataFrame lang vals = case lang of
--  R -> "data.frame(" ++ (concat $ L.intersperse "," fields) ++ ")"
--    where fields = map showRPair $ splitNamesFromDataFrameValues vals

--splitNamesFromDataFrameValues :: [ASValue] -> [(String, ASValue)]
--splitNamesFromDataFrameValues vals = pairs
--  where
--    pairs = if (all isString names)
--      then zip (map str names) (map (\(ValueL l) -> ValueL $ tail l) vals)
--      else zip (repeat ("" :: String)) vals
--    names = map (\(ValueL l) -> head l) vals
