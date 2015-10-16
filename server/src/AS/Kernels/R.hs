{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module AS.Kernels.R where

import AS.Types.Core

import AS.Kernels.Common
import AS.Kernels.LanguageUtils

import AS.Parsing.In (isHighDimensional)

import AS.Config.Settings
import AS.Util

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

execOnString :: EvalCode -> (EvalCode -> IO ASValue) -> IO ASValue
execOnString str f = do
  printWithTime "starting R eval"
  case (trimWhitespace R str) of
    "" -> return NoValue
    trimmed -> f trimmed

-- takes (isGlobalExecution, str)
execR :: Bool -> EvalCode -> IO ASValue
execR isGlobal s =
  let whenCaught = (\e -> return $ ValueError (show e) StdErr "" 0) :: (SomeException -> IO ASValue)
  in do
    putStrLn "got here"
    catch (R.runRegion $ (castR s) <$> if isGlobal
      then [r| eval(parse(text=s_hs)) |]
      else [r| AS_LOCAL_ENV<-function(){eval(parse(text=s_hs))};AS_LOCAL_ENV() |]) whenCaught

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

-- requires the input eval string, in order to return lists correctly
castR :: EvalCode -> R.SomeSEXP a -> ASValue
castR str (R.SomeSEXP s) =
  let vec = castSEXP str s
  in case (length vec) of
    1 -> head vec
    _ -> ValueL vec

-- everything returned is a list, because R.
castSEXP :: EvalCode -> R.SEXP s a -> [ASValue]
castSEXP str x = case x of
  (hexp -> H.Nil)       -> [ValueNull]
  (hexp -> H.Real v)    -> map ValueD $ SV.toList v
  (hexp -> H.Int v)     -> map (ValueI . fromIntegral) $ SV.toList v
  (hexp -> H.Logical v) -> map (ValueB . fromLogical) $ SV.toList v
  (hexp -> H.Char v)    -> [castString v]
  (hexp -> H.String v)  -> map (\(hexp -> H.Char c) -> castString c) $ SV.toList v
  (hexp -> H.Symbol s s' s'')  -> castSEXP s
  (hexp -> H.List a b c) -> (castSEXP str a) ++ (castSEXP str b) ++ (castSEXP str c)
  (hexp -> H.Special i) -> [ValueI $ fromIntegral i]
  (hexp -> H.DotDotDot s) -> castSEXP s
  (hexp -> H.Vector len v) -> castVector str v
  (hexp -> H.Builtin i) -> [ValueI $ fromIntegral i]
  (hexp -> H.Raw v) -> [ValueS . bytesToString $ SV.toList v]
  (hexp -> H.S4 s) -> castSEXP s
  _ -> [ValueError "Could not cast R value." StdErr "" 0]

castString :: SV.Vector s 'R.Char Word8 -> ASValue
castString v = ValueS . bytesToString $ SV.toList v

castVector :: EvalCode -> SV.Vector s SV.Vector (R.SomeSEXP s) -> [ASValue]
castVector v str = values
  where
    values = map castR $ SV.toList v
    (l, ls) = splitLastLine R str

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
