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

import AS.Types.Cell
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
  NoValue    -> LD.outNull lang
  ValueNaN   -> LD.outNan lang
  ValueInf   -> LD.outInf lang
  -- Excel strings shouldn't have quotes around them, during dragging inference, where this function is used
  ValueS s   -> case lang of
    Excel -> s
    otherwise -> show s
  ValueI i   -> show i
  ValueD d   -> show d
  ValueB b   -> LD.outBool lang b
  ValueSerialized s _ -> s
  _ -> error ("In showPrimitive, failed to pattern match: " ++ (show v))

showExpanding :: ASLanguage -> ExpandingValue -> String

showExpanding lang (VList coll) = wrapList lang $ showCollection lang coll

showExpanding R (VRList pairs) = "list(" ++ inner ++ ")"
  where 
    showRPair (key, arr) = (prefix key) ++ (showExpanding R $ VList . A $ arr)
    inner      = (concat $ L.intersperse "," $ map showRPair pairs)
    prefix key = case key of
      "" -> ""
      _ -> key ++ "="

showExpanding R (VRDataFrame labels indices vals) = "{function(){" ++ statements ++ "}}()"
  where
    statements = L.intercalate ";" [dfDef, labelDef, indexDef, "AS_DF_LOCAL"]
    dfDef      = "AS_DF_LOCAL <- data.frame(" ++ dfInner ++ ")"
    dfInner    = L.intercalate "," $ map ((showExpanding R) . VList . A) vals
    labelDef   = "colnames(AS_DF_LOCAL) <- " ++ (showExpanding R $ VList . A $ labels)
    indexDef   = "rownames(AS_DF_LOCAL) <- " ++ (showExpanding R $ VList . A $ indices)

showExpanding R (VPDataFrame labels indices vals) = showExpanding R (VRDataFrame labels indices (L.transpose vals)) -- python and R dataframes have transposed representations

showExpanding R (VNPArray coll) = wrapList R $ showCollection R coll

showExpanding R (VNPMatrix mat) = wrapList R $ showCollection R (M mat)

showExpanding R (VPSeries indices vals) = wrapList R $ showCollection R (A vals)

showExpanding Python (VNPArray coll) = "np.array(" ++ arrayVals ++ ")"
  where arrayVals = showCollection Python coll

showExpanding Python (VNPMatrix mat) = "np.matrix(" ++ matrixVals ++ ")"
  where matrixVals = showCollection Python $ M mat

showExpanding Python (VPDataFrame labels indices vals) = "pd.DataFrame(" ++ inner ++ ")"
  where 
    inner    = L.intercalate "," [vals', labels', indices']
    vals'    = "data=" ++ (showCollection Python $ M vals)
    labels'  = "columns=" ++ (showCollection Python $ A labels)
    indices' = "index=" ++ (showCollection Python $ A indices)

showExpanding Python (VRDataFrame labels indices vals) = showExpanding Python (VPDataFrame labels indices (L.transpose vals)) -- python and R dataframes have transposed representations

showExpanding Python (VPSeries indices vals) = "pd.Series(" ++ inner ++ ")"
  where 
    inner    = vals' ++ "," ++ indices'
    vals'    = "data=" ++ (showCollection Python $ A vals)
    indices' = "index=" ++ (showCollection Python $ A indices)

showExpanding l v = error $ "cannot insert value " ++ (show v) ++ " in language " ++ (show l)
    
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