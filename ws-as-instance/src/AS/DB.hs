{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
{-# CFILES db_odbc.c #-}
module AS.DB where

import AS.Types hiding (location,expression,value)
import Data.Maybe (isNothing)
import Prelude


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

--------------------------------------------------------------------------------------------------------------
-- | Handlers

handleGet :: ASPayload -> IO ASMessage
handleGet (PayloadLL locs) = do
    cells <- getCells locs
    if L.any isNothing cells
        then return failureMessage
        else return $ Message NoAction Success (PayloadCL (L.map (\(Just x)->x) cells))

handleDelete :: ASPayload -> IO ASMessage 
handleDelete (PayloadL loc) = deleteCell loc >> return successMessage
handleDelete (PayloadLL locs) = deleteCells locs >> return successMessage

--------------------------------------------------------------------------------------------------------------
-- | Types to convert from C structs to Haskell data types

data C_ASCell = C_Cell { location :: CString, expression :: CString, value :: CString} deriving Show
data C_ASCells = C_Cells {cells :: Ptr C_ASCell, numCells :: CInt} deriving Show
data C_ASRelation = C_Relation {fromLoc :: CString, toLoc :: CString}
data C_ASDAG = C_DAG {dag :: Ptr C_ASRelation, numEdges :: CInt}

--------------------------------------------------------------------------------------------------------------
-- | Storable instances to marshall C/Haskell types from pointers

instance Storable C_ASCell where
    sizeOf    _ = (24)
    alignment _ = alignment (undefined :: CString)

    poke p c = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ location c
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p $ expression c
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) p $ value c

    peek p = return C_Cell
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) p)
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 8) p)
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 16) p)

instance Storable C_ASCells where
    sizeOf    _ = (16)
    alignment _ = alignment (undefined :: CString)

    poke p c = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ cells c
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p $ numCells c

    peek p = return C_Cells
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) p)
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 8) p)

instance Storable C_ASRelation where
    sizeOf    _ = (16)
    alignment _ = alignment (undefined :: CString)

    poke p r = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ fromLoc r
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p $ toLoc r

    peek p = return C_Relation
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) p)
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 8) p)

instance Storable C_ASDAG where
    sizeOf    _ = (16)
    alignment _ = alignment (undefined :: CString)

    poke p d = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ dag d
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p $ numEdges d

    peek p = return C_DAG
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) p)
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 8) p)

--------------------------------------------------------------------------------------------------------------
-- | Function imports from db_odbc.c

foreign import ccall unsafe "db_odbc.c getCells" c_getCells :: Ptr CString -> CInt -> IO (Ptr C_ASCells)
foreign import ccall unsafe "db_odbc.c setCells" c_setCells :: Ptr CString -> Ptr CString -> Ptr CString -> CInt -> IO ()
foreign import ccall unsafe "db_odbc.c deleteEdges" c_deleteEdges :: Ptr CString -> CInt -> IO ()
foreign import ccall unsafe "db_odbc.c insertEdges" c_insertEdges :: Ptr CString -> Ptr CString -> CInt -> IO ()
foreign import ccall unsafe "db_odbc.c getEdges" c_getEdges :: IO (Ptr C_ASDAG)

--------------------------------------------------------------------------------------------------------------
-- | Conversion functions / helpers

convertCell :: C_ASCell -> IO (Maybe ASCell)
convertCell c = do 
  l <- peekCString (location c)
  e <- peekCString (expression c) 
  v <- peekCString (value c) 
  return $ Just $ Cell (read l :: ASLocation) (read e :: ASExpression) (read v :: ASValue)

convertRelation :: C_ASRelation -> IO (ASLocation,ASLocation)
convertRelation (C_Relation a b) = do 
  s1 <- peekCString a
  s2 <- peekCString b
  return ((read s1 :: ASLocation),(read s2 :: ASLocation))

-- | Converts Haskell array of strings to C-ptr
getPtr :: [String] -> IO (Ptr CString)
getPtr lst = do 
  cstrings <- mapM newCString lst
  newArray cstrings

--------------------------------------------------------------------------------------------------------------
-- | DB ASCell functions

deleteCell :: ASLocation -> IO ()
deleteCell loc = deleteCells [loc]

deleteCells :: [ASLocation] -> IO ()
deleteCells locs = return ()

getCells :: [ASLocation] -> IO [Maybe ASCell]
getCells [] = return []
getCells locs = do
  putStrLn $ "locs in get cells: " L.++ (show locs)
  l <- getPtr (L.map show locs)
  ptrCells <- c_getCells l (fromIntegral (L.length locs))
  ccells <- peek ptrCells
  arrCells <- peekArray (fromIntegral (numCells ccells)) (cells ccells)
  mapM convertCell arrCells

setCell :: ASCell -> IO ()
setCell c = setCells [c]

setCells :: [ASCell] -> IO ()
setCells c = do 
  locs <- getPtr $ L.map show (L.map cellLocation c)
  exprs <- getPtr $ L.map show (L.map cellExpression c)
  vals <- getPtr $ L.map show (L.map cellValue c)
  c_setCells locs exprs vals (fromIntegral (L.length c))

--------------------------------------------------------------------------------------------------------------
-- | DB DAG functions


getDAG :: IO [(ASLocation,ASLocation)]
getDAG = do 
  ptrDAG <- c_getEdges
  d <- peek ptrDAG
  putStrLn $ show (fromIntegral (numEdges d))
  arrEdges <- peekArray (fromIntegral (numEdges d)) (dag d)
  mapM convertRelation arrEdges

updateDAG :: [([ASLocation],ASLocation)] -> IO ()
updateDAG update = do 
  _ <- deleteDAGEdges (L.map snd update)
  let edgeList = L.concat (L.map (\(a,b) -> zip a (L.repeat b)) update)
  insertDAGEdges (L.map fst edgeList) (L.map snd edgeList)

deleteDAGEdges :: [ASLocation] -> IO ()
deleteDAGEdges toLocs = do 
  tl <- getPtr (L.map show toLocs)
  c_deleteEdges tl (fromIntegral (L.length toLocs))

insertDAGEdges :: [ASLocation] -> [ASLocation] -> IO ()
insertDAGEdges fromLocs toLocs = do 
  tl <- getPtr (L.map show toLocs)
  fl <- getPtr (L.map show fromLocs)
  c_insertEdges fl tl (fromIntegral (L.length toLocs))



-- hsc2hs DB.hsc -L -lodbc (ghc comes with hsc2hs)