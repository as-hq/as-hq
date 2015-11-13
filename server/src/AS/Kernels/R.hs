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

import Data.List (elem)

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
-- | Exposed functions

evaluate :: EvalCode -> EitherTExec ASValue
evaluate "" = return NoValue
evaluate str = liftIO $ execOnString str (execR False)


evaluateRepl :: EvalCode -> EitherTExec ASValue
evaluateRepl "" = return NoValue
evaluateRepl str = liftIO $ execOnString str (execR True)

--clearRepl :: IO ()
--clearRepl = do
--  R.runRegion $ do [r| rm(list=ls()) |]
--  return ()

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Helpers

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
  let whenCaught = (\e -> return . CellValue $ ValueError (show e) "R_EXEC_ERROR" "" 0) :: (SomeException -> IO CompositeValue)
  in do
    result <- catch (R.runRegion $ castR =<< if isGlobal
      then [r| eval(parse(text=s_hs)) |]
      else [r| AS_LOCAL_ENV<-function(){setwd(paste(getwd(),"/static",sep="")); result = eval(parse(text=s_hs)); setwd("../"); result}; AS_LOCAL_EXEC<-AS_LOCAL_ENV(); AS_LOCAL_EXEC |]) whenCaught
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

castR :: R.SomeSEXP a -> R a CompositeValue
castR (R.SomeSEXP s) = castSEXP s

-- everything returned is a list, because R.
castSEXP :: R.SEXP s a -> R s CompositeValue
castSEXP x = case x of
  (hexp -> H.Nil)       -> return $ CellValue NoValue
  (hexp -> H.Real v)    -> return . rdVector $ map fromReal $ SV.toList v
  (hexp -> H.Int v)     -> return . rdVector $ map (ValueI . fromIntegral) $ SV.toList v
  (hexp -> H.Logical v) -> return . rdVector $ map (ValueB . fromLogical) $ SV.toList v
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
  _ -> return $ CellValue $ ValueError "Could not cast R value." "R_Error" "" 0

rdVector :: [ASValue] -> CompositeValue
rdVector vals
  | length vals == 1 = CellValue (head vals) 
  | otherwise = Expanding . VList . A $ vals

castString :: SV.Vector s 'R.Char Word8 -> ASValue
castString v = ValueS . bytesToString $ SV.toList v

castVector :: SV.Vector s 'R.Vector (R.SomeSEXP s) -> R s CompositeValue
castVector v = do
  vals <- rdVectorVals <$> mapM castR $ SV.toList v
  (CellValue (ValueB isList)) <- castR =<< [r|is.list(AS_LOCAL_EXEC)|]
  (CellValue (ValueB isDf)) <- castR =<< [r|is.data.frame(AS_LOCAL_EXEC)|]
  if isList
    then if isDf
      then do
        listNames <- castListNames <$> castR =<< [r|names(AS_LOCAL_EXEC)|]
        return . Expanding $ VRDataFrame listNames vals
      else do
        listNames <- castListNames <$> (castR =<< [r|names(AS_LOCAL_EXEC)|])
        if (isRPlot listNames)
          then do
            uid <- liftIO getUniqueId
            path <- liftIO getImagesPath
            let imageUid = uid ++ ".png"
                imagePath = path ++ imageUid
            [r|ggsave(filename=imagePath_hs, plot=AS_LOCAL_EXEC)|]
            return . CellValue $ ValueImage imageUid
          else 
            let (A listVals) = vals
            in return . Expanding $ VRList (zip listNames listVals)
    else return . Expanding $ VList vals

--let firstRow = take (length vals) listNames'
--            vals' = prependRowToColumnMajorList firstRow vals

--prependRowToColumnMajorList :: [ASValue] -> [ASValue] -> [ASValue]
--prependRowToColumnMajorList row cols = cols'
--  where
--    cols' = map (\(rowElem,col) -> case col of
--      (ValueL l) -> ValueL $ rowElem:l
--      _ -> ValueL $ rowElem:col:[]) $ zip row cols

castListNames :: CompositeValue -> [String]
castListNames val = case val of
  (Expanding (Vlist (A names))) -> map (\(ValueS s)->s) names
  (CellValue (ValueS s)) -> [s]
  (CellValue NoValue)  -> ["NULL"]

-- TODO figure out S4 casting
castS4 :: R.SEXP s a -> R s ASValue
castS4 s = return $ ValueError "S4 objects not currently supported." "R_Error" "" 0

fromLogical :: R.Logical -> Bool
fromLogical R.TRUE  = True
fromLogical R.FALSE = False
fromLogical R.NA    = False

fromReal :: Double -> ASValue
fromReal d = case (fromDouble d) of
  Left dDouble -> ValueD dDouble
  Right dInt -> ValueI dInt

--sanitizeList :: ASValue -> ASValue
--sanitizeList v = if (isHighDimensional 0 v)
--  then ValueError "Cannot embed lists of dimension > 2." "R_Error" "" 0
--  else undegenerateList v

--undegenerateList :: ASValue -> ASValue
--undegenerateList (ValueL [l]) = l 
--undegenerateList v = v

--(<.) :: R a -> R ASValue
--(<.) a = castR =<< a

isRPlot :: [RListKey] -> Bool
isRPlot = elem "plot_env"
