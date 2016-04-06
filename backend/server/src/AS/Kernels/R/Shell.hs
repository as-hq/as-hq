{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module AS.Kernels.R.Shell where

import Data.List (transpose, dropWhile, dropWhileEnd)
import Data.List.Split (chunksOf)
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
import AS.Logging
import AS.Util 
import AS.Types.Cell hiding (Cell)
import AS.Types.Eval
import AS.Types.Errors
import AS.Types.Sheets
import AS.Kernels.Internal
import AS.Kernels.R.Types

type VarSymbol = String

-----------------------------------------------------------------------------------------------------------------------------
-- Exposed functions

newShell :: IO Shell
newShell = R.runRegion $ do
  -- the app needs to be run as root to install packages.
  [r|
    library("rjson")
    library("ggplot2")
  |]

  return $ Shell M.empty

runBlock :: MVar State -> EvalCode -> EvalScope -> ASSheetId -> IO EvalResult
runBlock state code scope sid = do
  curState <- readMVar state
  cwd <- S.getSetting S.appDirectory
  uid <- getUniqueId
  let imageName = uid ++ ".png"
  let imagePath = cwd ++ "/static/images/" ++ imageName

  let onException :: SomeException -> IO EvalResult
      onException e = do
        puts state $ "runBlock ERROR: " ++ show e
        return $ EvalResult 
          (CellValue (ValueError (show e) "R error"))
          (Just $ "Runtime error: " ++ (show e))

  handleAny onException $ 
    possiblyOverrideWithImage imageName =<< (
      R.runRegion $ do
        when (M.notMember sid (curState^.shell.environments)) $ 
          installEnv state sid
        curState <- liftIO $ readMVar state
        let runCode = prepareExpression scope imagePath code
        let againstEnv :: SEXP a 'R.Env
            againstEnv = R.sexp $ (curState^.shell.environments) M.! sid
        execR [r| eval(parse(text=runCode_hs), envir=againstEnv_hs) |]
    )

clear :: MVar State -> ASSheetId -> IO ()
clear state sid = runRegion $ installEnv state sid

-----------------------------------------------------------------------------------------------------------------------------
-- Util

trimWhitespace :: ASLanguage -> String -> String  -- TODO use the language to get block delimiters
trimWhitespace lang = dropWhileEnd isWhitespace . dropWhile isWhitespace
  where isWhitespace c = (c == ' ') || (c == '\n') || (c == '\t') || (c == ';')

prepareExpression :: EvalScope -> FilePath -> EvalCode -> EvalCode
prepareExpression scope imagePath code = 
  let code' = "\npng(" ++ show imagePath ++ ")" ++ 
              "\nresult = tryCatch(eval(parse(text=" ++ show code ++ ")), finally = { dev.off() } )" ++ 
              "\nresult"
  in case scope of 
    Cell -> isolateCodeScope code'
    Header -> code'
      
isolateCodeScope :: EvalCode -> EvalCode 
isolateCodeScope code = "(function() {" ++ code ++ "})()"
  
-----------------------------------------------------------------------------------------------------------------------------
-- Exec helpers

installEnv :: MVar State -> ASSheetId -> R a ()
installEnv state sid = do
  newEnvUnformed <- [r|new.env()|]
  liftIO $ do
    let newEnv = R.cast R.SEnv newEnvUnformed
    R.preserveObject newEnv -- preserve across all gc sweeps (for multithreaded access)
    modifyMVar_' state $ return . (& shell.environments %~ M.insert sid (R.unsexp newEnv))
    puts state $ "Created new environment for sheet: " ++ T.unpack sid

-- TODO stacktraces (fill in the second argument of EvalResult)
execR :: R m (R.SomeSEXP m) -> R m EvalResult
execR ex = ex >>= castR >>= (\cv -> return $ EvalResult cv Nothing)

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
  (hexp -> H.Real v)    -> return . rdVector $ map fromReal $ SV.toList v
  (hexp -> H.Int v)     -> return . rdVector $ map (ValueI . fromIntegral) $ SV.toList v
  (hexp -> H.Logical v) -> return . rdVector $ map fromLogical $ SV.toList v
  (hexp -> H.Char v)    -> return $ CellValue (castString v)
  (hexp -> H.String v)  -> return . rdVector $ map (\(hexp -> H.Char c) -> castString c) $ SV.toList v
  (hexp -> H.Symbol s s' s'') -> castSEXP $ R.SomeSEXP s
  -- (hexp -> H.List car cdr tag) -> return . concat =<< mapM castSEXP [car, cdr, tag] 
  -- ^ this case only fires on pairlists, due to HaskellR issue #214
  (hexp -> H.Special i)    -> return $ CellValue (ValueI $ fromIntegral i)
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
            uid <- liftIO getUniqueId
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
  Expanding (VList (A vals)) <- castSEXP s
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
  | d == fromInteger dInt = ValueI dInt
  | otherwise             = ValueD d
  where dInt = round d

isRGgPlot :: [RListKey] -> Bool
isRGgPlot = elem "plot_env"