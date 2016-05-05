{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module AS.Kernels.R.Shell where

import Data.List (transpose, dropWhile, dropWhileEnd)
import Data.List.Split (chunksOf)
import Data.Char (isSpace)
import Data.Word (Word8)
import System.Directory
import Control.Monad.IO.Class
import Control.Applicative
import Control.Exception (handle, SomeException)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Lens
import Control.Concurrent
import System.Directory (getCurrentDirectory)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.Text as T

import qualified Data.Vector.SEXP as SV
import Foreign.C.String (peekCString)
import Foreign.Storable (peek)
import Foreign.Marshal.Array (peekArray)
import Language.Haskell.TH.Ppr (bytesToString)

import qualified Foreign.R as R
import Foreign.R.Type as R
import Foreign.R (SEXP, SEXPTYPE, printValue)
import Language.R.Instance as R
import Language.R as LR
import Language.R.QQ
import Language.R.HExp as H
import Control.Memory.Region

import AS.Prelude
import AS.Config.Settings as S
import AS.Util 
import AS.Types.Cell hiding (Cell, isInfinite)
import AS.Types.Eval hiding (isInfinite)
import AS.Types.Errors
import AS.Types.Sheets
import AS.Kernels.Internal
import AS.Kernels.R.Types

-----------------------------------------------------------------------------------------------------------------------------
-- Exposed functions

runBlock :: EvalScope -> R.SEXP0 -> EvalCode -> IO EvalResult
runBlock scope env code = do
  cwd <- S.getSetting S.appDirectory
  uid <- getUUID
  let imageName = uid ++ ".png"
  let imagePath = cwd ++ S.images_dir ++ imageName

  let onException :: SomeException -> IO EvalResult
      onException e = do
        putStrLn $ "runBlock ERROR: " ++ show e
        return $ EvalResult 
          (CellValue (ValueError (show e) "R error"))
          (Just $ show e)

  handleAny onException $ 
    possiblyOverrideWithImage imageName =<< do
      R.runRegion $ do
        let runCode = prepareExpression scope imagePath code
        let formedEnv = R.sexp env :: SEXP a 'R.Env
        execR [r| eval(parse(text=runCode_hs), envir=formedEnv_hs) |]

-----------------------------------------------------------------------------------------------------------------------------
-- Util

trimWhitespace :: ASLanguage -> String -> String  -- TODO use the language to get block delimiters
trimWhitespace lang = dropWhileEnd isWhitespace . dropWhile isWhitespace
  where isWhitespace c = isSpace c || (c == ';')

prepareExpression :: EvalScope -> FilePath -> EvalCode -> EvalCode
prepareExpression scope imagePath code = 
  case scope of 
    Cell    -> isolateCodeScope code'
    Header  -> code'
  where 
    code' = unlines
      [ "png(" ++ show imagePath ++ ")"
      , "f <- file(open = \"w+\", blocking = FALSE)"
      , "sink(file=f, append=TRUE, type=c(\"output\"))"
      , "result <- list()"
      , "result$value <- tryCatch(eval(parse(text=" ++ show code ++ ")), finally = { dev.off() } )"
      , "result$display <- readLines(f)"
      , "sink()"
      , "close(f)"
      , "result"
      ]
      
isolateCodeScope :: EvalCode -> EvalCode 
isolateCodeScope code = "(function() {" ++ code ++ "})()"
  
-----------------------------------------------------------------------------------------------------------------------------
-- Exec helpers

execR :: R m (R.SomeSEXP m) -> R m EvalResult
execR ex = do
  result <- ex
  value <- castR =<< [r| result_hs$value |]
  disp <- castR =<< [r| result_hs$display |]
  return $ case disp of 
    -- for multiple printed lines, readLines() returns a string vector
    Expanding (VList (A d)) -> EvalResult value d'
      where 
        d' = case d of 
          [] -> Nothing
          ds -> Just . unlines $ map fromValueS ds
        fromValueS (ValueS s) = s
    -- for a single printed line, readLines() returns a char vector
    CellValue (ValueS s) -> EvalResult value (Just s)
    -- all else (e.g. Nil) should be ignored 
    _ -> EvalResult value Nothing

-----------------------------------------------------------------------------------------------------------------------------
-- Casting helpers

castR :: R.SomeSEXP m -> R m CompositeValue
castR s = do
  CellValue (ValueB isMatrix) <- castSEXP =<< [r|is.matrix(s_hs)|]
  if isMatrix
    then castMatrix s
    else castSEXP s

-- | In the R code, png(imagePath_hs) means that any image generated gets saved into the file
-- named imagePath (within the directory of fpStatic). If any code generates an image without returning
-- an error, we override the return value of that code with the image. 
possiblyOverrideWithImage :: String -> EvalResult -> IO EvalResult
possiblyOverrideWithImage imageName (EvalResult cv disp) = do 
  exist <- doesFileExist $ S.images_dir ++ imageName
  let cv' = if exist 
            then CellValue . ValueImage $ imageName
            else cv 
  return $ EvalResult cv' disp

castSEXP :: R.SomeSEXP m -> R m CompositeValue
castSEXP origValue@(R.SomeSEXP x) = case x of
  (hexp -> H.Nil)       -> return $ CellValue NoValue
  (hexp -> H.Real v)    -> return . rdVector . map fromReal $ SV.toList v
  (hexp -> H.Int v)     -> return . rdVector . map (ValueI . fromIntegral) $ SV.toList v
  (hexp -> H.Logical v) -> return . rdVector . map fromLogical $ SV.toList v
  (hexp -> H.Char v)    -> return $ CellValue (castString v)
  (hexp -> H.String v)  -> return . rdVector . map (\(hexp -> H.Char c) -> castString c) $ SV.toList v
  (hexp -> H.Symbol s s' s'') -> castSEXP $ R.SomeSEXP s
  -- (hexp -> H.List car cdr tag) -> return . concat =<< mapM castSEXP [car, cdr, tag] 
  -- ^ this case only fires on pairlists, as described in HaskellR issue #214
  (hexp -> H.Special i)    -> return $ CellValue (ValueI $ fromIntegral i)
  (hexp -> H.Closure _ _ _) -> return $ CellValue NoValue -- we don't want to display functions to user
  (hexp -> H.DotDotDot s)  -> castSEXP $ R.SomeSEXP s
  (hexp -> H.Vector len v) -> castVector origValue v
  (hexp -> H.Builtin i)    -> return $ CellValue (ValueI $ fromIntegral i)
  (hexp -> H.Raw v)        -> return $ CellValue (ValueS . bytesToString $ SV.toList v)
  (hexp -> H.S4 s)         -> CellValue <$> castS4 s
  _ -> return . CellValue $ ValueError "Could not cast R value." "R Error"

rdVector :: [ASValue] -> CompositeValue
rdVector vals
  | length vals == 1 = CellValue $ $head vals 
  | otherwise        = Expanding . VList . A $ vals

castString :: SV.Vector s 'R.Char Word8 -> ASValue
castString v = ValueS . bytesToString $ SV.toList v

castVector :: R.SomeSEXP m -> SV.Vector m 'R.Vector (R.SomeSEXP m) -> R m CompositeValue
castVector origValue vec = do
  vals <- rdVectorVals <$> mapM castR (SV.toList vec)
  (CellValue (ValueB isList)) <- castR =<< [r|is.list(origValue_hs)|]
  (CellValue (ValueB isDf)) <- castR =<< [r|is.data.frame(origValue_hs)|]

  if isList
    then if isDf
      then do
        names <- castNames <$> (castR =<< [r|names(origValue_hs)|])
        indices <- castNames <$> (castR =<< [r|rownames(origValue_hs)|])
        return . Expanding $ VRDataFrame names indices (transpose vals)
      else do
        listNames <- castNames <$> (castR =<< [r|names(origValue_hs)|])
        let nameStrs = map (\(ValueS s) -> s) listNames
        if (isRGgPlot nameStrs)
          then do
            uid <- liftIO getUUID
            let imageName = uid ++ ".png"
                savePath = S.images_dir ++ imageName
            [r|ggsave(filename=savePath_hs, plot=origValue_hs)|]
            return . CellValue $ ValueImage imageName
          else return . Expanding . VRList $ zip nameStrs vals
          -- ggplot plots generated within functions don't save, so we're adding
          -- special case code here to deal with this. 
          -- http://stackoverflow.com/questions/7034647/save-ggplot-within-a-function
    else return . Expanding . VList . M $ vals

castMatrix :: R.SomeSEXP m -> R m CompositeValue
castMatrix s = do
  elems <- castSEXP s
  let vals = case elems of
                Expanding (VList (A vs)) -> vs
                CellValue v              -> [v] 
                _ -> $error $ "could not cast matrix vals for: " ++ show elems
  Expanding (VList (A dims)) <- castR =<< [r|dim(s_hs)|]
  let [(ValueI nrows), _] = dims
  return . Expanding . VList . M $ transpose' $ chunksOf (fromInteger nrows) vals

rdVectorVals :: [CompositeValue] -> Matrix
rdVectorVals = map mkArray
  where
    mkArray row = case row of 
      Expanding (VList (A arr)) -> arr
      CellValue v               -> [v]
      _ -> [ValueError "cannot cast multi-dimensional vector" "R Error"]

castNames :: CompositeValue -> [ASValue]
castNames val = case val of
  Expanding (VList (A names)) -> names
  CellValue (ValueS s)        -> [ValueS s]
  CellValue NoValue           -> [ValueS "NULL"]
  _ -> $error $ "could not cast dataframe labels from composite value " ++ (show val)

-- TODO figure out S4 casting
castS4 :: SEXP m a -> R m ASValue
castS4 s = return $ ValueError "S4 objects not currently supported." "R Error"

fromLogical :: R.Logical -> ASValue
fromLogical R.TRUE  = ValueB True
fromLogical R.FALSE = ValueB False
fromLogical R.NA    = ValueNaN

fromReal :: Double -> ASValue
fromReal d 
  | isNaN d               = ValueNaN
  | isInfinite d          = ValueInf
  | d == fromInteger dInt = ValueI dInt
  | otherwise             = ValueD d
  where dInt = round d

isRGgPlot :: [RListKey] -> Bool
isRGgPlot = elem "plot_env"