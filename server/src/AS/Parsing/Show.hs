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
import qualified AS.LanguageDefs as LD

-----------------------------------------------------------------------------------------------------------------------
-- exposed

showValue :: ASLanguage -> CompositeValue -> String
showValue lang v = case v of 
  CellValue v' -> showPrimitive lang v'
  Expanding v' -> showExpanding lang v'

showPrimitive :: ASLanguage -> ASValue -> String
showPrimitive lang v = case v of
  NoValue            -> LD.outNull lang
  ValueNaN           -> LD.outNan lang
  ValueS s           -> show s
  ValueI i           -> show i
  ValueD d           -> show d
  ValueB b           -> LD.outBool lang b
  _ -> error ("In showPrimitive, failed to pattern match: " ++ (show v))

showExpanding :: ASLanguage -> ExpandingValue -> String

showExpanding lang (VList coll) = wrapList lang $ showCollection lang coll

showExpanding R (VRList pairs) = "list(" ++ (concat $ L.intersperse "," $ map showRPair pairs) ++ ")"
  where 
    showRPair (key, arr) = (prefix key) ++ (showExpanding R $ VList . A $ arr)
    prefix key = case key of
      "" -> ""
      _ -> key ++ "="

--showExpanding R (VRDataFrame )

showExpanding Python (VNPArray coll) = "np.array(" ++ arrayVals ++ ")"
  where arrayVals = showCollection Python coll

showExpanding Python (VNPMatrix mat) = "np.matrix(" ++ matrixVals ++ ")"
  where matrixVals = showCollection Python $ M mat

showExpanding Python (VPDataFrame labels indices vals) = "pd.DataFrame(" ++ inner ++ ")"
  where 
    inner = L.intercalate "," [vals', labels', indices']
    vals' = "data=" ++ (showCollection Python $ M vals)
    labels' = "columns=" ++ (showCollection Python $ A labels)
    indices' = "index=" ++ (showCollection Python $ A indices)

showExpanding Python (VPSeries indices vals) = "pd.Series(" ++ inner ++ ")"
  where 
    inner = vals' ++ "," ++ indices'
    vals' = "data=" ++ (showCollection Python $ A vals)
    indices' = "index=" ++ (showCollection Python $ A indices)
    
-----------------------------------------------------------------------------------------------------------------------
-- helpers

showCollection :: ASLanguage -> Collection -> String
showCollection lang coll = case coll of 
  A arr -> list lang $ map (showPrimitive lang) arr
  M mat -> list lang $ map (\row -> list lang $ map (showPrimitive lang) row) mat

wrapList :: ASLanguage -> String -> String
wrapList lang l = wrapL ++ l ++ wrapR
  where
    (wrapL, wrapR) = case lang of 
      Python -> ("arr(", ")")
      _ -> ("", "") 

list :: ASLanguage -> [String] -> String
list lang xs = start ++ (L.intercalate dlm xs) ++ end
  where
    (start, end)   = LD.listStops lang
    dlm            = (LD.listDelimiter lang):[] 

mkObject :: ObjectType -> [JSONPair] -> String
mkObject otype pairs = "deserialize({\"tag\":\"Object\",\"objectType\":\"" ++ otype' ++ "\"," ++ pairs' ++ "})"
  where
    otype' = show otype
    pairs' = L.intercalate "," $ map showPair pairs
    showPair pair = (fst pair) ++ ":" ++ (snd pair)

--object :: ASLanguage -> ASValue -> String
--object lang (ValueObject otype olist oattrs) =
--  let 
--    dlm = LD.blockDelimiter lang
--    olist' = showValue lang (ValueL olist)
--  in foldr (++) $ case lang of
--    Python  -> ["deserialize(", (show otype), ",", olist, (show oattrs), ")", dlm]
--    _ -> error "cannot deserialize object in languages other than python"

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
