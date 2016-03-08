{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module AS.Kernels.R
  ( evaluate
  , evaluateRepl
  , evaluateHeader
  ) where

import Data.List (transpose, dropWhile, dropWhileEnd)
import Data.List.Split (chunksOf)
import Data.Word (Word8)
import System.Directory
import Control.Monad.IO.Class
import Control.Applicative
import Control.Exception (handle, SomeException)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import System.Directory (getCurrentDirectory)
import qualified Data.ByteString.Char8 as BC

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

import AS.Prelude
import Prelude()
import AS.Config.Settings as S
import AS.Logging
import AS.Util 
import AS.Types.Cell
import AS.Types.Eval
import AS.Types.Errors
import AS.Types.Sheets
import AS.Kernels.Internal

-----------------------------------------------------------------------------------------------------------------------------
-- Exposed functions

-- Don't actually need the sheet id's as arguments right now. (Alex 11/15)
evaluate :: String -> EvalCode -> EitherTExec EvalResult
evaluate _ ""  = return emptyResult
evaluate _ str = do
  v <- liftIO $ execOnString str (execR False)
  return $ EvalResult v Nothing

evaluateRepl :: EvalCode -> EitherTExec EvalResult
evaluateRepl ""  = return emptyResult
evaluateRepl str = do
  v <- liftIO $ execOnString str (execR True)
  return $ EvalResult v Nothing

evaluateHeader :: EvalCode -> EitherTExec EvalResult
evaluateHeader str = do 
  lift clearRepl
  evaluateRepl str

clearRepl :: IO ()
clearRepl = do
 R.runRegion $ castR =<< [r| rm(list=ls()) |]
 return ()

-----------------------------------------------------------------------------------------------------------------------------
-- Util

trimWhitespace :: ASLanguage -> String -> String  -- TODO use the language to get block delimiters
trimWhitespace lang = dropWhileEnd isWhitespace . dropWhile isWhitespace
  where isWhitespace c = (c == ' ') || (c == '\n') || (c == '\t') || (c == ';')

-----------------------------------------------------------------------------------------------------------------------------
-- Exec helpers

execOnString :: EvalCode -> (EvalCode -> IO CompositeValue) -> IO CompositeValue
execOnString str f = do
  printWithTime "starting R eval"
  case (trimWhitespace R str) of
    "" -> return $ CellValue NoValue
    trimmed -> f trimmed

-- takes (current project dir, isGlobalExecution, str)
-- change wd and then change back so that things like read.table will read from the static folder
execR :: Bool -> EvalCode -> IO CompositeValue
execR isGlobal s = do 
  fp <- S.getSetting S.appDirectory
  let fpStatic = S.static_dir
      onException :: SomeException -> IO CompositeValue
      onException e = do
        R.runRegion $ castR =<< [r| setwd(fp_hs) |]
        return . CellValue $ ValueError (show e) "R error"
  uid <- liftIO getUniqueId
  -- See comment under possiblyOverrideWithImage for how images work
  let imageName = uid ++ ".png"
      imagePath = "images/" ++ imageName
  handle onException $ 
    possiblyOverrideWithImage imageName =<< (R.runRegion $ castR =<< if isGlobal
      then [r| eval(parse(text=s_hs)) |]
      else [r| 
      AS_LOCAL_ENV <- function() {
        setwd(fpStatic_hs) 
        png(imagePath_hs)
        result = eval(parse(text=s_hs))
        dev.off()
        setwd(fp_hs)
        result
      }; 
      AS_LOCAL_EXEC<-AS_LOCAL_ENV()
      AS_LOCAL_EXEC 
      |])

-- @anand faster unboxing, but I can't figure out how to restrict x to (IsVector x)
--castR :: (IsVector a) => (R.SomeSEXP (R.SEXP (Control.Memory.Region s) a) -> IO ASValue
--castR (R.SomeSEXP x) =
  --let offset = R.length x
  --case x of
    --(hexp -> H.Real _)    -> (map ValueD) <$> (peekArray offset =<< R.real x)
    --(hexp -> H.Int _)     -> (map (ValueI . fromIntegral)) <$> (peekArray offset =<< R.integer x)
    --(hexp -> H.Logical _) -> (map (ValueB . fromLogical)) <$> (peekArray offset =<< R.logical x)
    --(hexp -> H.Char _)    -> (\s -> [ValueS s]) <$> (peekCString =<< R.char x)
    --(hexp -> H.String _)  -> (map ValueS) <$> (mapM peekCString =<< mapM R.char =<< peekArray offset =<< R.string x)

-----------------------------------------------------------------------------------------------------------------------------
-- Casting helpers

castR :: R.SomeSEXP a -> R a CompositeValue
castR s@(R.SomeSEXP x) = do
  (CellValue (ValueB isMatrix)) <- (\(R.SomeSEXP b) -> castSEXP b) =<< [r|is.matrix(s_hs)|]
  if isMatrix
    then castMatrix x
    else castSEXP x

-- | In the R code, png(imagePath_hs) means that any image generated gets saved into the file
-- named imagePath (within the directory of fpStatic). If any code generates an image without returning
-- an error, we override the return value of that code with the image. 
possiblyOverrideWithImage :: String -> CompositeValue -> IO CompositeValue
possiblyOverrideWithImage imageName cv = do 
  exist <- doesFileExist $ S.images_dir ++ imageName
  return $ if exist 
    then CellValue . ValueImage $ imageName
    else cv 

castSEXP :: R.SEXP s a -> R s CompositeValue
castSEXP x = case x of
  (hexp -> H.Nil)       -> return $ CellValue NoValue
  (hexp -> H.Real v)    -> return . rdVector $ map fromReal $ SV.toList v
  (hexp -> H.Int v)     -> return . rdVector $ map (ValueI . fromIntegral) $ SV.toList v
  (hexp -> H.Logical v) -> return . rdVector $ map fromLogical $ SV.toList v
  (hexp -> H.Char v)    -> return $ CellValue (castString v)
  (hexp -> H.String v)  -> return . rdVector $ map (\(hexp -> H.Char c) -> castString c) $ SV.toList v
  (hexp -> H.Symbol s s' s'') -> castSEXP s
  -- (hexp -> H.List car cdr tag) -> return . concat =<< mapM castSEXP [car, cdr, tag] 
  -- ^ this case only fires on pairlists, due to HaskellR issue #214
  (hexp -> H.Special i)    -> return $ CellValue (ValueI $ fromIntegral i)
  (hexp -> H.DotDotDot s)  -> castSEXP s
  (hexp -> H.Vector len v) -> castVector v
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

castVector :: SV.Vector s 'R.Vector (R.SomeSEXP s) -> R s CompositeValue
castVector v = do
  vals <- rdVectorVals <$> (mapM castR $ SV.toList v)
  (CellValue (ValueB isList)) <- castR =<< [r|is.list(AS_LOCAL_EXEC)|]
  (CellValue (ValueB isDf)) <- castR =<< [r|is.data.frame(AS_LOCAL_EXEC)|]

  if isList
    then if isDf
      then do
        names <- castNames <$> (castR =<< [r|names(AS_LOCAL_EXEC)|])
        indices <- castNames <$> (castR =<< [r|rownames(AS_LOCAL_EXEC)|])
        return . Expanding $ VRDataFrame names indices (transpose vals)
      else do
        listNames <- castNames <$> (castR =<< [r|names(AS_LOCAL_EXEC)|])
        let nameStrs = map (\(ValueS s) -> s) listNames
        if (isRGgPlot nameStrs)
          then do
            uid <- liftIO getUniqueId
            let imageName = uid ++ ".png"
                savePath = S.images_dir ++ imageName
            [r|ggsave(filename=savePath_hs, plot=AS_LOCAL_EXEC)|]
            return . CellValue $ ValueImage imageName
          else return . Expanding . VRList $ zip nameStrs vals
          -- ggplot plots generated within functions don't save, so we're adding
          -- special case code here to deal with this. 
          -- http://stackoverflow.com/questions/7034647/save-ggplot-within-a-function
    else return . Expanding . VList . M $ vals

castMatrix :: R.SEXP s a -> R s CompositeValue
castMatrix x = do
  (Expanding (VList (A vals))) <- castSEXP x
  (Expanding (VList (A dims))) <- castR =<< [r|dim(AS_LOCAL_EXEC)|]
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
castS4 :: R.SEXP s a -> R s ASValue
castS4 s = return $ ValueError "S4 objects not currently supported." "R Error"

fromLogical :: R.Logical -> ASValue
fromLogical R.TRUE  = ValueB True
fromLogical R.FALSE = ValueB False
fromLogical R.NA    = ValueNaN

fromReal :: Double -> ASValue
fromReal d 
  | d == fromInteger dInt = ValueI dInt
  | otherwise             = ValueD d
  where dInt = round d

isRGgPlot :: [RListKey] -> Bool
isRGgPlot = elem "plot_env"