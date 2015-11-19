{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module AS.Kernels.R where

import AS.Types.Core

import AS.Kernels.Common
import AS.Kernels.LanguageUtils

import AS.Config.Settings
import AS.Config.Paths (getImagesPath)
import AS.Util

import Data.List (elem, transpose)

import qualified Data.ByteString.Char8 as BC

import Foreign.C.String (peekCString)
import Foreign.Storable (peek)
import Foreign.Marshal.Array (peekArray)

import Language.Haskell.TH.Ppr (bytesToString)

import qualified Foreign.R as R
import Foreign.R.Type as R
import Foreign.R (SEXP, SEXPTYPE)
import Language.R.Instance as R
import Language.R as LR
import Language.R.QQ
import Language.R.HExp as H

import qualified Data.Vector.SEXP as SV
import Data.Word (Word8)

--import Control.Monad (msum)
import Control.Monad.IO.Class
import Control.Applicative
import Control.Exception (catch, SomeException)
-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

----------------------------------------------------------------------------------------------------------------------------------------------
-- Exposed functions

evaluate :: EvalCode -> EitherTExec CompositeValue
evaluate "" = return $ CellValue NoValue
evaluate str = liftIO $ execOnString str (execR False)


evaluateRepl :: EvalCode -> EitherTExec CompositeValue
evaluateRepl "" = return $ CellValue NoValue
evaluateRepl str = liftIO $ execOnString str (execR True)

evaluateHeader :: ASSheetId -> EvalCode -> EitherTExec ASValue
evaluateHeader sid str = do 
  lift $ writeHeaderFile sid R str
  lift clearRepl
  evaluateRepl sid str 

clearRepl :: IO ()
clearRepl = do
 R.runRegion $ castR =<< do [r| rm(list=ls()) |]
 return ()

----------------------------------------------------------------------------------------------------------------------------------------------
-- Exec helpers

execOnString :: EvalCode -> (EvalCode -> IO CompositeValue) -> IO CompositeValue
execOnString str f = do
  printWithTime "starting R eval"
  case (trimWhitespace R str) of
    "" -> return $ CellValue NoValue
    trimmed -> f trimmed

-- takes (isGlobalExecution, str)
-- change wd and then change back so that things like read.table will read from the static folder
execR :: Bool -> EvalCode -> IO CompositeValue
execR isGlobal s =
<<<<<<< HEAD
  let whenCaught :: SomeException -> IO ASValue
      whenCaught e = (R.runRegion $ castR =<< [r| setwd("../") |]) >> (return (ValueError (show e) "R_EXEC_ERROR" "" 0))
=======
  let whenCaught :: SomeException -> IO CompositeValue
      whenCaught e = return . CellValue $ ValueError (show e) "R_EXEC_ERROR" 
>>>>>>> objects
  in do
    result <- catch (R.runRegion $ castR =<< if isGlobal
      then [r| eval(parse(text=s_hs)) |]
      else [r| AS_LOCAL_ENV<-function(){eval(parse(text=s_hs));}; AS_LOCAL_EXEC<-AS_LOCAL_ENV(); AS_LOCAL_EXEC |]) whenCaught
    return result

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

----------------------------------------------------------------------------------------------------------------------------------------------
-- Casting helpers

castR :: R.SomeSEXP a -> R a CompositeValue
castR (R.SomeSEXP s) = castSEXP s

castSEXP :: R.SEXP s a -> R s CompositeValue
castSEXP x = case x of
  (hexp -> H.Nil)       -> return $ CellValue NoValue
  (hexp -> H.Real v)    -> return . rdVector $ map fromReal $ SV.toList v
  (hexp -> H.Int v)     -> return . rdVector $ map (ValueI . fromIntegral) $ SV.toList v
  (hexp -> H.Logical v) -> return . rdVector $ map fromLogical $ SV.toList v
  (hexp -> H.Char v)    -> return $ CellValue (castString v)
  (hexp -> H.String v)  -> return . rdVector $ map (\(hexp -> H.Char c) -> castString c) $ SV.toList v
  (hexp -> H.Symbol s s' s'')  -> castSEXP s
  --(hexp -> H.List car cdr tag) -> return . concat =<< mapM castSEXP [car, cdr, tag] -- this case only fires on pairlists, due to HaskellR issue #214
  (hexp -> H.Special i) -> return $ CellValue (ValueI $ fromIntegral i)
  (hexp -> H.DotDotDot s) -> castSEXP s
  (hexp -> H.Vector len v) -> castVector v
  (hexp -> H.Builtin i) -> return $ CellValue (ValueI $ fromIntegral i)
  (hexp -> H.Raw v) -> return $ CellValue (ValueS . bytesToString $ SV.toList v)
  (hexp -> H.S4 s) -> CellValue <$> castS4 s
  _ -> return . CellValue $ ValueError "Could not cast R value." "R Error"

rdVector :: [ASValue] -> CompositeValue
rdVector vals
  | length vals == 1 = CellValue (head vals) 
  | otherwise = Expanding . VList . A $ vals

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
        if (isRPlot nameStrs)
          then do
            uid <- liftIO getUniqueId
            path <- liftIO getImagesPath
            let imageUid = uid ++ ".png"
                imagePath = path ++ imageUid
            [r|ggsave(filename=imagePath_hs, plot=AS_LOCAL_EXEC)|]
            return . CellValue $ ValueImage imageUid
          else return . Expanding . VRList $ zip nameStrs vals
    else return . Expanding . VList . M $ vals

rdVectorVals :: [CompositeValue] -> Matrix
rdVectorVals = map mkArray
  where
    mkArray row = case row of 
      Expanding (VList (A arr)) -> arr
      CellValue v -> [v]
      _ -> error "cannot cast multi-dimensional vector"

castNames :: CompositeValue -> [ASValue]
castNames val = case val of
  Expanding (VList (A names)) -> names
  CellValue (ValueS s) -> [ValueS s]
  CellValue NoValue  -> [ValueS "NULL"]
  _ -> error $ "could not cast dataframe labels from composite value " ++ (show val)

-- TODO figure out S4 casting
castS4 :: R.SEXP s a -> R s ASValue
castS4 s = return $ ValueError "S4 objects not currently supported." "R Error"

fromLogical :: R.Logical -> ASValue
fromLogical R.TRUE  = ValueB True
fromLogical R.FALSE = ValueB False
fromLogical R.NA    = ValueNaN

fromReal :: Double -> ASValue
fromReal d = case (fromDouble d) of
  Left dDouble -> ValueD dDouble
  Right dInt -> ValueI dInt

isRPlot :: [RListKey] -> Bool
isRPlot = elem "plot_env"
