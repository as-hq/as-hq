{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module AS.Kernels.Python.Client where

import Data.Maybe (fromJust)
import Data.List (findIndex, intercalate)
import Data.Aeson 
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Control.Exception (catch, SomeException)
import System.ZMQ4.Monadic
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import qualified Data.List.Utils as LU
import Database.Redis (Connection)

import AS.Prelude
import AS.Logging
import AS.Types.Cell hiding (Cell)
import AS.Types.Eval
import AS.Types.EvalHeader
import AS.Types.Errors
import AS.Types.Sheets
import AS.Types.CondFormat
import AS.Types.Network
import AS.Types.Messages (MessageId)
import qualified AS.Util as U

import AS.Kernels.Internal
import AS.Kernels.Python.Types
import AS.Logging
import AS.Config.Settings
import AS.Config.Constants
import qualified AS.Parsing.Read as R
import AS.Parsing.Show
import AS.Parsing.Common
import AS.DB.API (getAllHeaders)

--------------------------------------------------------------------------------
-- Initialization 

initialize :: Connection -> IO ()
initialize conn = do
  -- Run all the headers in db to initialize the sheet namespaces
  headers <- getAllHeaders conn Python
  forM_ headers $ \h -> do 
    let wid  = h^.evalHeaderWorkbookId
    let expr = h^.evalHeaderExpr
    runEitherT $ evaluateHeader initialize_message_id wid expr

--------------------------------------------------------------------------------
-- Top level evaluation functions

evaluate :: MessageId -> WorkbookID -> EvalCode -> EitherTExec EvalResult
evaluate = evaluateWithScope Cell

evaluateHeader :: MessageId -> WorkbookID -> EvalCode -> EitherTExec EvalResult
evaluateHeader = evaluateWithScope Header

-- #needsrefactor Should not hard core errors
evaluateLambdaFormat :: WorkbookID -> 
                        LambdaConditionExpr -> 
                        ASValue -> 
                        EitherTExec FormatResult
evaluateLambdaFormat wid lambdaExpr val = do 
  (mFormatStr, mErr) <- case val of 
    ValueError _ _ -> return (Nothing, Just "can't format cell with error")
    v              -> do 
      let evalExpr = "(" ++ lambdaExpr ++ ")(" ++ 
                      showValue Python (CellValue v) ++ ")"
      EvaluateFormatReply f e <- runRequest $ EvaluateFormatRequest wid evalExpr
      return (f,e)
  return $ case mFormatStr of 
    Just formatStr -> case R.parseFormatValue formatStr of 
      Nothing -> FormatError "Failed to interpret Python result as string." 
      Just format -> FormatSuccess format
    Nothing  -> case mErr of 
      Just err -> FormatError err
      Nothing  -> FormatError "Formatting returned neither value nor error."

haltMessage :: MessageId -> IO ()
haltMessage = runRequest_ . HaltMessageRequest

clear :: WorkbookID -> IO ()
clear = runRequest_ . ClearRequest

-- | SQL code is converted to Python code, so simply evaluating as usual will 
-- suffice for now.
evaluateSql :: MessageId -> WorkbookID -> EvalCode -> EitherTExec EvalResult
evaluateSql = evaluateWithScope Cell

--------------------------------------------------------------------------------
-- General Helpers

testCell :: WorkbookID -> EvalCode -> IO ()
testCell wid code = do 
  evalledCell <- runEitherT $ evaluate wid test_message_id code
  putsObj "Test evaluate python cell: " evalledCell

testHeader :: WorkbookID -> EvalCode -> IO ()
testHeader wid code = do 
  evalledHeader <- runEitherT $ evaluateHeader wid test_message_id code
  putsObj "Test evaluate python header: " evalledHeader

--------------------------------------------------------------------------------
-- Evaluation helpers

evaluateWithScope :: EvalScope -> 
                     MessageId -> 
                     WorkbookID -> 
                     EvalCode -> 
                     EitherTExec EvalResult
evaluateWithScope _ _ _ "" = return emptyResult
evaluateWithScope scope mid wid code = do
  (EvaluateReply v err disp) <- runRequest $ EvaluateRequest scope mid wid code
  let cleanedDisp = cleanDisplay <$> disp
  case v of 
    Nothing -> case err of 
      Just e -> return $ EvalResult (CellValue $ ValueError e "") cleanedDisp
      Nothing -> return $ EvalResult (CellValue NoValue) cleanedDisp
    Just v -> do
      cval <- hoistEither $ R.parseValue Python (BC.pack v)
      return $ EvalResult cval cleanedDisp

-- | Cleans out error messages that the Python kernel adds to the cell output. 
-- before cleaning: http://puu.sh/o1hQF/e64250da19.png
-- after cleaning: http://puu.sh/o1hQC/9d721cbfca.png
-- 
-- Note: the more correct way of doing this is to override the IPython kernel's 
-- error reporting settings. This code appears to do most of the stuff though.
cleanDisplay :: String -> String
cleanDisplay cellDisplay = cellDisplay'
  where 
    lines = T.lines $ T.pack cellDisplay
    errStart = "Traceback (most recent call last)"
    errEnd = "<alphasheets-input-"
    mErrStart = findIndex (T.isInfixOf errStart) lines
    mErrEnd = findIndex (T.isInfixOf errEnd) lines
    lines' = case (mErrStart, mErrEnd) of 
      (Just errStart, Just errEnd) -> if errStart < errEnd 
        then [l | (l, i) <- zip lines [0,1..], i < errStart || i >= errEnd]
        else lines
      _ -> lines 
    cellDisplay' = T.unpack . T.unlines $ lines' 

--------------------------------------------------------------------------------
-- ZMQ run requests

-- | Connect as a client, send the KernelRequest and get a KernelReply using ZMQ
-- protocols.
runRequest :: KernelRequest -> EitherTExec KernelReply
runRequest msg = do
  resp <- liftIO $ runZMQ $ do
    addr <- liftIO $ getSetting pykernelAddress
    dealer <- connectToKernel addr
    let mMid = getMessageId msg
    let send = send' dealer [] $ encode msg
    let poke = getPoke msg dealer
    let defErr = GenericErrorReply "Python Kernel Down!"
    let isStill = (==) StillProcessingReply
    getResponse mMid send poke isStill defErr eitherDecodeStrict dealer
  case resp of 
    Left e -> left $ KernelError e
    -- this is a top-level kernel error that should throw a "left"
    -- i.e. an API error, network error, or other non-evaluation-related error
    Right (GenericErrorReply e) -> left $ KernelError e 
    Right r -> return r

-- | Connect to the kernel as a client and send the kernel a message as a 
-- new client, without returning a response. Still attempt persistence.
runRequest_ :: KernelRequest -> IO ()
runRequest_ msg = void $ do
  resp <- runEitherT $ runRequest msg
  case resp of
    Right (GenericErrorReply e) -> $error e
    otherwise -> return ()

-- | Given the request and the client, return the poke action to test if
--  the message sent to the kernel is still being processed. 
getPoke ::  KernelRequest -> Socket z Dealer -> ZMQ z ()
getPoke req dealer = case getMessageId req of
  Just mid -> send' dealer [] $ encode (GetStatusRequest mid)
  _ -> return ()

-- | Helper to get message ids
getMessageId :: KernelRequest -> Maybe MessageId
getMessageId (EvaluateRequest _ mid _ _) = Just mid
getMessageId (EvaluateFormatRequest _ _) = Nothing
getMessageId (ClearRequest _) = Nothing
getMessageId (HaltMessageRequest mid) = Just mid
getMessageId (GetStatusRequest mid) = Just mid

--------------------------------------------------------------------------------