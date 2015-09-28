{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
module DB where

import Prelude
import GHC.Generics
import Data.Text


import Control.Applicative
import Control.Monad

import Foreign
import Foreign.C.Types
import Foreign.C.String(CString(..))
import Foreign.C
import qualified Data.List as L

import Data.Text.Unsafe

import System.IO.Unsafe (unsafePerformIO)
import Data.Vector.Storable hiding (mapM)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Sheets

type ASSheetId = Text
data ASSheet = Sheet {sheetId :: ASSheetId, sheetName :: String} deriving (Show, Read, Eq)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Core cell types

data ASIndex = Index {locSheetId :: ASSheetId, index :: (Int, Int)} deriving (Show, Read, Eq, Generic, Ord)
data ASRange = Range {locSheetId :: ASSheetId, range :: ((Int, Int), (Int, Int))} deriving (Show, Read, Eq, Generic, Ord)
data ASColumn = Column {locSheetId :: ASSheetId, column :: Int} deriving (Show, Read, Eq, Generic, Ord)
data ASLocation = IndexLoc ASIndex | RangeLoc ASRange | ColumnLoc ASColumn

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
  deriving (Show, Read, Eq)

type ASEvalError = String

data ASLanguage = R | Python | OCaml | CPP | Java | SQL | Excel deriving (Show, Read, Eq)

-- TODO consider migration to exLocs record
data ASExpression =
  Expression { expression :: String, language :: ASLanguage } | 
  Reference { location :: ASIndex, referenceIndex :: (Int, Int) }
  deriving (Show, Read, Eq)

data ASCellTag = 
  Color String |
  Size Int |
  Money |
  Percentage |
  Tracking |
  Volatile 
  deriving (Show, Read, Eq)

data ASCell = Cell {cellLocation :: ASIndex, 
					cellExpression :: ASExpression,
					cellValue :: ASValue,
          cellTags :: [ASCellTag]} deriving (Show, Read, Eq)

----------------

data C_ASCell = C_Cell {cLocation :: CString, cExpression :: CString, cValue :: CString, cTags :: CString} deriving Show
data C_ASCells = C_Cells {cells :: Ptr C_ASCell, numCells :: CInt} deriving Show

--------------------------------------------------------------------------------------------------------------
-- | Storable instances to marshall C/Haskell types from pointers

instance Storable C_ASCell where
    sizeOf    _ = (32)
    alignment _ = alignment (undefined :: CString)

    poke p c = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ cLocation c
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p $ cExpression c
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) p $ cValue c
        (\hsc_ptr -> pokeByteOff hsc_ptr 24) p $ cTags c


    peek p = return C_Cell
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) p)
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 8) p)
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 16) p)
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 24) p)

instance Storable C_ASCells where
    sizeOf    _ = (16)
    alignment _ = alignment (undefined :: CString)

    poke p c = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ cells c
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p $ numCells c

    peek p = return C_Cells
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) p)
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 8) p)


foreign import ccall unsafe "hiredis/redis_db.c getCells" c_getCells :: Ptr CString -> CInt -> IO (Ptr C_ASCells)
foreign import ccall unsafe "hiredis/redis_db.c setCells" c_setCells :: Ptr C_ASCell -> CInt -> IO ()

convertCell :: C_ASCell -> IO (Maybe ASCell)
convertCell c = do 
  l <- peekCString (location c)
  e <- peekCString (expression c) 
  v <- peekCString (value c) 
  return $ Just $ Cell (read l :: ASIndex) (read e :: ASExpression) (read v :: ASValue)

-- | Converts Haskell array of strings to C-ptr
getPtr :: [String] -> IO (Ptr CString)
getPtr lst = do 
  cstrings <- mapM newCString lst
  newArray cstrings


getCells :: [ASIndex] -> IO [Maybe ASCell]
getCells [] = return []
getCells locs = do
  putStrLn $ "locs in get cells: " L.++ (show locs)
  l <- getPtr (L.map show locs)
  ptrCells <- c_getCells l (fromIntegral (L.length locs))
  cASCells <- peek ptrCells
  cCells <- peekArray (fromIntegral (numCells cASCells)) (cells cASCells)
  res <- mapM convertCell cCells
  free (cells cASCells)
  free (ptrCells)
  return res

