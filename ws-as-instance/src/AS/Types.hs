{-# LANGUAGE DeriveGeneric #-}

module AS.Types where

import Prelude
import GHC.Generics
import Data.Aeson hiding (Success)
import Data.Text
import qualified Network.WebSockets as WS

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Core cell types

type ASSheetId = Text
data ASSheet = Sheet {sheetId :: ASSheetId, sheetName :: String} deriving (Show, Read, Eq, Generic, Ord)

data ASLocation = Index {sheet :: ASSheet, index :: (Int, Int)} | 
                  Range {sheet :: ASSheet, range :: ((Int, Int), (Int, Int))}
                  deriving (Show, Read, Eq, Generic, Ord)

data ASValue =
  NoValue |
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

data ASCellTag = 
  Color String |
  Size Int |
  Money |
  Percentage |
  Streaming {streamSource :: StreamSource, streamFrequency :: Int} |
  Tracking
  deriving (Show, Read, Eq, Generic)

data ASCell = Cell {cellLocation :: ASLocation, 
					cellExpression :: ASExpression,
					cellValue :: ASValue,
          cellTags :: [ASCellTag]} deriving (Show, Read, Eq, Generic)


-- TODO fix recursion
data ExLoc = ExSheet {name :: String, sheetLoc :: ExLoc} |
             ExRange {first :: ExLoc, second :: ExLoc}     |
             ExIndex {d1 :: String, col :: String, d2 :: String, row :: String} deriving (Show,Read,Eq,Ord)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Streaming

data StreamSource = 
  Bloomberg {url :: String, key :: String} -- example stream source TODO rewrite based on actual API
  deriving (Show, Read, Eq, Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Message Types

data ASMessage = Message {
  messageUserId :: ASUserId,
  action :: ASAction,
  result :: ASResult,
  payload :: ASPayload
} deriving (Show, Read, Eq, Generic)

data ASAction = 
  NoAction |
  Acknowledge |
  Evaluate | 
  Update |
  Get |
  Delete |
  Undo |
  Redo |
  Commit |
  Clear | 
  UpdateWindow |
  Tag
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
  PayloadL ASLocation |
  PayloadLL [ASLocation] |
  PayloadW ASWindow |
  PayloadCommit ASCommit
  deriving (Show, Read, Eq, Generic)

data ASInitConnection = ASInitConnection {connUserId :: ASUserId} deriving (Show,Read,Eq,Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Eval Types

data ASExecError = 
  Timeout | 
  DependenciesLocked {lockUserId :: ASUserId} | 
  DBNothingException | 
  NetworkDown | 
  ResourceLimitReached 
  deriving (Show, Read, Eq, Generic)

type EitherCells = Either ASExecError [ASCell] 

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Websocket types

data ASWindow = Window {windowSheetId :: ASSheetId, topLeft :: (Int, Int), bottomRight :: (Int, Int)} deriving (Show, Read, Eq, Generic)
type ASUserId = Text 
data ASUser = User {userId :: ASUserId, userConn :: WS.Connection, userWindows :: [ASWindow]} 
type ServerState = [ASUser]

instance Eq ASUser where 
  c1 == c2 = (userId c1) == (userId c2)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Version Control

data ASTime = Time {day :: String, hour :: Int, min :: Int, sec :: Int} deriving (Show,Read,Eq,Generic)
data ASCommit = ASCommit {commitUserId :: ASUserId, before :: [ASCell], after :: [ASCell], time :: ASTime} deriving (Show,Read,Eq,Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Convenience methods

lst :: ASValue -> [ASValue]
lst (ValueL l) = l
lst other = [other]

str :: ASValue -> String
str (ValueS s) = s

dbl :: ASValue -> Double
dbl (ValueD d) = d

failureMessage :: ASMessage
failureMessage = Message (pack "testUserId") NoAction (Failure "generic") (PayloadN ())
-- TODO get user id

initialViewingWindow :: ASWindow
initialViewingWindow = Window "testSheetId" (0, 0) (100, 100)
-- TODO generate Unique sheet id

genericText :: Text
genericText = pack ""

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
instance ToJSON ASExecError
instance FromJSON ASExecError
instance FromJSON ASTime
instance ToJSON ASTime
instance ToJSON ASCommit 
instance FromJSON ASCommit
instance ToJSON ASCellTag
instance FromJSON ASCellTag
instance ToJSON ASWindow
instance FromJSON ASWindow
instance ToJSON ASSheet
instance FromJSON ASSheet
instance ToJSON StreamSource
instance FromJSON StreamSource