{-# LANGUAGE OverloadedStrings #-}

module AS.Parsing.Read where

import Control.Applicative
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Word8 as W
import Safe (readMay)
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Map as M

import Prelude()
import AS.Prelude hiding (takeWhile)
import AS.Types.Cell
import AS.Types.CellProps
import AS.Types.Eval
import AS.Util

import AS.Parsing.Common as PC
import qualified AS.LanguageDefs as LD

-----------------------------------------------------------------------------------------------------------------------
-- Primitive parsers

complain = fail "could not parse complex/object/json value"

nullValue :: ASLanguage -> Parser ASValue
nullValue lang = case lang of 
  Python -> PC.string' (LD.inNull Python) >> return NoValue
  _      -> fail $ "No nullValue in " ++ show lang

nanValue :: ASLanguage -> Parser ASValue
nanValue lang = case lang of 
  Python -> PC.string' (LD.inNan Python) >> return ValueNaN
  _      -> fail $ "No NaN value in " ++ show lang

infValue :: ASLanguage -> Parser ASValue
infValue lang = case lang of 
  Python -> PC.string' (LD.inInf Python) >> return ValueInf
  _      -> fail $ "No Inf value in " ++ show lang

cellJsonValue :: ASLanguage -> Parser ASValue
cellJsonValue lang = f =<< json lang
  where f js = maybe complain return $ extractCellValue js

-----------------------------------------------------------------------------------------------------------------------
-- Composite parsers

-- | Parser for looking up arbitrary fields
(.>) :: JSON -> JSONKey -> Maybe JSONField
(.>) = flip M.lookup

-- | Parser for looking up strings
(.$>) :: JSON -> JSONKey -> Maybe String
(.$>) js key = (\(JSONLeaf (SimpleValue (ValueS s))) -> s) <$> M.lookup key js

parseComposite :: ASLanguage -> Parser CompositeValue
parseComposite lang = 
      f =<< json lang
  <|> complain
  where f js = case js .$> "tag" of 
            Just tag -> maybe complain return $ extractCompositeValue tag js
            Nothing -> fail "expecting field \"tag\" in complex value"

extractCompositeValue :: JSONKey -> JSON -> Maybe CompositeValue
extractCompositeValue tag js = case tag of 
  "CellValue" -> CellValue <$> extractCellValue js
  "Expanding" -> Expanding <$> extractExpanding js

extractCellValue :: JSON -> Maybe ASValue
extractCellValue js = case js .$> "cellValueType" of 
  Just "Image"      -> extractImage js 
  Just "Error"      -> extractError js
  Just "Serialized" -> extractSerialized js
  _ -> Nothing

extractExpanding :: JSON -> Maybe ExpandingValue
extractExpanding js = 
  let readEType e = $read e :: ExpandingType
  in case readEType <$> js .$> "expandingType" of 
    Just List -> VList <$> extractCollection js "listVals"
    Just NPArray -> VNPArray <$> extractCollection js "arrayVals"
    Just NPMatrix -> (\(M mat) -> VNPMatrix mat) <$> extractCollection js "matrixVals"
    Just PDataFrame -> Just $ VPDataFrame labels indices vals
      where
        (Just (A labels)) = extractCollection js "dfLabels"
        (Just (A indices)) = extractCollection js "dfIndices"
        (Just (M vals)) = extractCollection js "dfData"
    Just PSeries -> Just $ VPSeries indices vals
      where 
        (Just (A indices)) = extractCollection js "seriesIndices"
        (Just (A vals)) = extractCollection js "seriesData"
    _ -> Nothing

extractSerialized :: JSON -> Maybe ASValue
extractSerialized js = ValueSerialized <$> (js .$> "serializedValue") <*> (js .$> "displayName")

extractImage :: JSON -> Maybe ASValue
extractImage js = ValueImage <$> (js .$> "imagePath")  

extractError :: JSON -> Maybe ASValue
extractError js = ValueError <$> (js .$> "errorMsg") <*> (js .$> "errorType") 

extractCollection :: JSON -> JSONKey -> Maybe Collection
extractCollection js key = case js .> key of 
  Just (JSONLeaf (ListValue collection)) -> Just collection
  _ -> Nothing 

extractNestedListItems :: JSON -> JSONKey -> Maybe [JSON]
extractNestedListItems js key = case js .> key of 
  Just (JSONLeaf (NestedListValue jsList)) -> Just jsList
  _ -> Nothing 

-----------------------------------------------------------------------------------------------------------------------
-- Low-level parsers

json :: ASLanguage -> Parser JSON
json lang = M.fromList <$> (braces $ sepBy pair delimiter)
  where
    delimiter  = PC.betweenSpaces $ string ","
    leaf       = JSONLeaf <$> jsonValue lang
    tree       = JSONTree <$> json lang
    braces     = PC.between' "{" "}" 
    pair      = do
      spaces
      key <- PC.unescapedString
      betweenSpaces $ string ":"
      field <- tree <|> leaf
      spaces
      return (C.unpack key, field)

jsonValue :: ASLanguage -> Parser JSONValue
jsonValue lang = 
      ListValue <$> list lang
  <|> SimpleValue <$> asValue lang
  -- | NestedList is called upon when the JSON value being parsed is a list of JSON
  -- Example:
  -- > parse (jsonValue Python) "" "[{'tag': 1}]}" 
  -- >  = Right (NestedListValue [fromList [("tag",JSONLeaf (SimpleValue (ValueI 1)))]])
  <|> NestedListValue <$> nestedList lang

parseList :: ASLanguage -> Parser a -> Parser [a]
parseList lang p = brackets $ sepBy p delimiter
  where
    (start, end) = LD.listStops lang
    brackets x   = PC.string' start *> x <* PC.string' end
    delimiter    = betweenSpaces $ PC.string' [LD.listDelimiter lang]

nestedList :: ASLanguage -> Parser [JSON]
nestedList lang = parseList lang $ json lang

-- | Parser for lists, which only allows 1D and 2D lists
list :: ASLanguage -> Parser Collection
list lang = 
      (A <$> (array value))
  <|> (M <$> (array $ array value))
  where array = parseList lang
        value = asValue lang

-----------------------------------------------------------------------------------------------------------------------
-- Top level parsers

parseValue :: ASLanguage -> ByteString -> Either ASExecError CompositeValue
parseValue lang = readOutput . parse (value lang)
  where
    readOutput (Fail _ _ _) = Left ParseError
    readOutput (Partial _) = Left ParseError
    readOutput (Done _ r)   = Right r

parseFormatValue :: String -> Maybe [CellProp]
parseFormatValue = readMay

value :: ASLanguage -> Parser CompositeValue
value lang = 
      CellValue <$> asValue lang
  <|> parseComposite lang

asValue :: ASLanguage -> Parser ASValue
asValue lang =
      ValueD <$> PC.float
  <|> ValueI <$> PC.integer
  <|> ValueB <$> PC.bool
  <|> ValueS . C.unpack <$> PC.unescapedString
  <|> nullValue lang
  <|> nanValue lang
  <|> infValue lang
  <|> cellJsonValue lang