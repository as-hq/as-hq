{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

module AS.Kernels.R where

import AS.Types.Core

import AS.Kernels.Common
import AS.Kernels.LanguageUtils

import AS.Parsing.In (isHighDimensional)

import AS.Config.Settings
import AS.Util

import qualified Data.ByteString.Char8 as BC

import Foreign.C.String
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

import Control.Monad.IO.Class
import Control.Applicative
-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Exposed functions

evaluate :: String -> EitherTExec ASValue
evaluate "" = return NoValue
evaluate str = liftIO $ execOnString str (execR False)


evaluateRepl :: String -> EitherTExec ASValue
evaluateRepl "" = return NoValue
evaluateRepl str = liftIO $ execOnString str (execR True)

--clearRepl :: IO ()
--clearRepl = do
--  R.runRegion $ do [r| rm(list=ls()) |]
--  return ()

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Helpers

execOnString :: String -> (String -> IO ASValue) -> IO ASValue
execOnString str f = do
  printWithTime "starting R eval"
  case (trimWhitespace R str) of
    "" -> return NoValue
    trimmed -> f trimmed

-- takes (isGlobalExecution, str)
execR :: Bool -> String -> IO ASValue
execR isGlobal s = do
  putStrLn "got here"
  R.runRegion $ liftIO . castR =<< if isGlobal
    then [r| eval(parse(text=s_hs)) |]
    else [r| AS_LOCAL_ENV<-function(){eval(parse(text=s_hs))};AS_LOCAL_ENV() |]

-- faster unboxing, but I can't figure out how to restrict x to (IsVector x)
--castR :: (IsVector a) => (R.SomeSEXP (R.SEXP (Control.Memory.Region s) a) -> IO ASValue
--castR (R.SomeSEXP x) =
  --case x of
    --(hexp -> H.Real _)    -> (map ValueD) <$> (peekArray offset =<< R.real x)
    --(hexp -> H.Int _)     -> (map (ValueI . fromIntegral)) <$> (peekArray offset =<< R.integer x)
    --(hexp -> H.Logical _) -> (map (ValueB . fromLogical)) <$> (peekArray offset =<< R.logical x)
    --(hexp -> H.Char _)    -> (\s -> [ValueS s]) <$> (peekCString =<< R.char x)
    --(hexp -> H.String _)  -> (map ValueS) <$> (mapM peekCString =<< mapM R.char =<< peekArray offset =<< R.string x)

castR :: R.SomeSEXP a -> IO ASValue
castR (R.SomeSEXP s) = do
  --offset <- R.length x
  let castSEXP x = case x of
            (hexp -> H.Real v)    -> map ValueD $ SV.toList v
            (hexp -> H.Int v)     -> map (ValueI . fromIntegral) $ SV.toList v
            (hexp -> H.Logical v) -> map (ValueB . fromLogical) $ SV.toList v
            (hexp -> H.Char v)    -> (\s -> [ValueS s]) . bytesToString $ SV.toList v
            (hexp -> H.String v)  -> map (\s -> case s of
              (hexp -> H.Char cv) -> ValueS . bytesToString $ SV.toList cv) $ SV.toList v
            (hexp -> H.List )
            _ -> [ValueError "Could not cast R value." StdErr "" 0]
  let vec = castSEXP s
  putStrLn $ "R got value: " ++ (show vec)
  return $ case (length vec) of
    1 ->  head vec
    _ -> ValueL vec

fromLogical :: R.Logical -> Bool
fromLogical R.TRUE  = True
fromLogical R.FALSE = False
fromLogical R.NA    = False

sanitizeList :: ASValue -> ASValue
sanitizeList v = if (isHighDimensional 0 v)
  then ValueError "Cannot embed lists of dimension > 2." StdErr "" 0
  else undegenerateList v

undegenerateList :: ASValue -> ASValue
undegenerateList v@(ValueL l) = if (length l == 1)
  then undegenerateList (head l)
  else v
undegenerateList v = v
