{-# LANGUAGE DeriveGeneric #-}

module AS.Types where

import Prelude
import GHC.Generics
import Data.Aeson hiding (Success)
import Data.Text
import qualified Network.WebSockets as WS

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Core cell types

data ASLocation = Index {sheet :: String, index :: (Int, Int)} | 
                  Range {sheet :: String, range :: ((Int, Int), (Int, Int))}
  deriving (Show, Read, Eq, Ord, Generic)

data ASValue =
  ValueNaN () |
  ValueS String |
  ValueI Int |
  ValueD Double | 
  ValueB Bool |
  ValueL [ASValue] |
  ExcelSheet { locs :: ASValue, exprs :: ASValue, vals :: ASValue} |
  Rickshaw {rickshawData :: ASValue} |
  ValueError { error :: String, err_type :: String, file :: String, position :: Int } | 
  ValueImage { imagePath :: String } |
  StockChart { stockPrices :: ASValue, stockName :: String } |
  ObjectValue { objectType :: String, jsonRepresentation :: String } |
  StyledValue { style :: String, value :: ASValue } |
  DisplayValue { displayValue :: String, actualValue :: ASValue }|
  ValueE ASEvalError
  deriving (Show, Read, Eq, Generic)

type ASEvalError = String

data ASLanguage = R | Python | OCaml | CPP | Java | SQL | Excel deriving (Show, Read, Eq, Generic)

data ASExpression =
  Expression { expression :: String, language :: ASLanguage } | 
  Reference { location :: ASLocation, referenceIndex :: (Int, Int) }
  deriving (Show, Read, Eq, Generic)


data ASCell = Cell {cellLocation :: ASLocation, 
					cellExpression :: ASExpression,
					cellValue :: ASValue} deriving (Show, Read, Eq, Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Message Types

data ASMessage = Message {
  action :: ASAction,
  result :: ASResult,
  payload :: ASPayload
} deriving (Show, Read, Eq, Generic)

data ASAction = 
  NoAction |
  Acknowledge |
  Evaluate | 
  Get |
  Delete |
  Undo |
  Redo |
  Commit |
  Clear
  deriving (Show, Read, Eq, Generic)

data ASResult = 
  Success | 
  Failure {failureDesc :: String} |
  NoResult
  deriving (Show, Read, Eq, Generic)

data ASPayload = 
  PayloadN () |
  PayloadInit ASInitConnection |
  PayloadC ASCell | 
  PayloadCL [ASCell] | 
  PayloadLL {getLocs::[ASLocation],vWindow:: ASViewingWindow} |
  PayloadCommit ASCommit
  deriving (Show, Read, Eq, Generic)

data ASInitConnection = ASInitConnection {userName :: Text} deriving (Show,Read,Eq,Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Eval Types

data ASViewingWindow = ASViewingWindow {vwTopLeftCol :: Int, vwTopLeftRow :: Int, vwWidth :: Int, vwHeight :: Int} deriving (Show,Read,Eq,Generic)

data ASExecError = 
  Timeout | 
  DependenciesLocked {userLock :: ASUser} | 
  DBNothingException | 
  NetworkDown | 
  ResourceLimitReached 
  deriving (Show, Read, Eq, Generic)

type ASEitherCells = Either ASExecError [ASCell] 

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Websocket types

data Client = Client {clientName :: Text, clientConn :: WS.Connection, clientVW :: ASViewingWindow} 
type ServerState = [Client]

instance Eq Client where 
  c1 == c2 = (clientName c1) == (clientName c2)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Version Control

data ASTime = Time {day :: String, hour :: Int, minute :: Int, second :: Int} deriving (Show,Read,Eq,Generic)
type ASUser = String
data ASCommit = ASCommit {user :: ASUser, before :: [ASCell], after :: [ASCell], time :: ASTime} deriving (Show,Read,Eq,Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Convenience methods

lst :: ASValue -> [ASValue]
lst (ValueL l) = l
lst other = [other]

str :: ASValue -> String
str (ValueS s) = s

dbl :: ASValue -> Double
dbl (ValueD d) = d

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Generic From/To JSON instances

instance ToJSON ASLocation
instance FromJSON ASLocation
instance ToJSON ASValue
instance FromJSON ASValue
instance ToJSON ASLanguage
instance FromJSON ASLanguage
instance ToJSON ASExpression
instance FromJSON ASExpression
instance ToJSON ASCell
instance FromJSON ASCell
instance ToJSON ASAction
instance FromJSON ASAction
instance ToJSON ASResult
instance FromJSON ASResult
instance ToJSON ASPayload
instance FromJSON ASPayload
instance ToJSON ASMessage
instance FromJSON ASMessage
instance ToJSON ASInitConnection
instance FromJSON ASInitConnection 
instance ToJSON ASViewingWindow
instance FromJSON ASViewingWindow 
instance ToJSON ASExecError
instance FromJSON ASExecError
instance FromJSON ASTime
instance ToJSON ASTime
instance ToJSON ASCommit 
instance FromJSON ASCommit