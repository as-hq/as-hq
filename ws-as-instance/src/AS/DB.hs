{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
{-# CFILES hiredis/as_db.c #-}
module AS.DB where

import AS.Types	hiding (location,expression,value)
import Data.Maybe (isNothing)
import Prelude
import AS.Util

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
handleGet (PayloadLL locs) = return $ Message genericText Get Success (PayloadCL [])

-- | not yet implemented
handleDelete :: ASPayload -> IO ASMessage 
handleDelete p@(PayloadLL locs) = return $ Message genericText Delete Success p

handleClear :: IO ASMessage
handleClear = return $ Message genericText Clear Success (PayloadN ())

handleUndo :: IO ASMessage
handleUndo = return $ Message genericText Undo Success (PayloadN ()) 

handleRedo :: IO ASMessage
handleRedo = return $ Message genericText Redo Success (PayloadN ()) 

pushCommit :: ASCommit -> IO ()
pushCommit commit = return ()

-------------- boilerplate ------------------------------
getDAG :: IO [(ASLocation,ASLocation)]
getDAG = return []

deleteCell :: ASLocation -> IO ()
deleteCell loc = return ()

deleteCells :: [ASLocation] -> IO ()
deleteCells locs = return ()

getCells :: [ASLocation] -> IO [ASCell]
getCells locs = return []

setCell :: ASCell -> IO ()
setCell c = return ()

setCells :: [ASCell] -> IO ()
setCells cells = return ()

updateDAG :: [([ASLocation],ASLocation)] -> IO ()
updateDAG update = return ()

--handleGet :: ASPayload -> IO ASMessage
--handleGet (PayloadLL locs vWindow) = do
--  cells <- getCells locs
--  if (L.length cells /= L.length locs)
--      then do 
--        failDesc <- (generateErrorMessage DBNothingException)
--        return $ Message Get (Failure failDesc) (PayloadN ())
--      else return $ Message Get Success (PayloadCL cells)

---- | not yet implemented
--handleDelete :: ASPayload -> IO ASMessage 
--handleDelete p@(PayloadLL locs) = (deleteCells locs) >> (return (Message Delete Success p)

--handleClear :: IO ASMessage
--handleClear = do 
--  c <- c_clear
--  return $ Message Clear Success (PayloadN ())

--handleUndo :: IO ASMessage
--handleUndo = do 
--  cs <- c_undo
--  s <- peekCString cs
--  return $ Message Undo Success (PayloadCommit (read s :: ASCommit)) 

--handleRedo :: IO ASMessage
--handleRedo = do 
--  cs <- c_redo
--  s <- peekCString cs
--  return $ Message Redo Success (PayloadCommit (read s :: ASCommit)) 



----------------------------------------------------------------------------------------------------------------
---- | Types to convert from C structs to Haskell data types

--data C_ASCell = C_Cell { location :: CString, expVal :: CString} deriving Show
--data C_ASCells = C_Cells {cells :: Ptr C_ASCell, numCells :: CInt} deriving Show
--data C_ASRelation = C_Relation {fromLoc :: CString, toLoc :: CString} deriving Show
--data C_ASDAG = C_DAG {dag :: Ptr C_ASRelation, numEdges :: CInt}

--data ExpVal = ExpVal {redisExp :: ASExpression, redisVal :: ASValue} deriving (Show,Eq,Read)

----------------------------------------------------------------------------------------------------------------
---- | Storable instances to marshall C/Haskell types from pointers

--instance Storable C_ASCell where
--    sizeOf    _ = (16)
--    alignment _ = alignment (undefined :: CString)

--    poke p c = do
--        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ location c
--        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p $ expVal c
--        -- (\hsc_ptr -> pokeByteOff hsc_ptr 16) p $ value c

--    peek p = return C_Cell
--              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) p)
--              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 8) p)
--              --`ap` ((\hsc_ptr -> peekByteOff hsc_ptr 16) p)

--instance Storable C_ASCells where
--    sizeOf    _ = (16)
--    alignment _ = alignment (undefined :: CString)

--    poke p c = do
--        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ cells c
--        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p $ numCells c

--    peek p = return C_Cells
--              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) p)
--              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 8) p)

--instance Storable C_ASRelation where
--    sizeOf    _ = (16)
--    alignment _ = alignment (undefined :: CString)

--    poke p r = do
--        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ fromLoc r
--        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p $ toLoc r

--    peek p = return C_Relation
--              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) p)
--              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 8) p)

--instance Storable C_ASDAG where
--    sizeOf    _ = (16)
--    alignment _ = alignment (undefined :: CString)

--    poke p d = do
--        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ dag d
--        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p $ numEdges d

--    peek p = return C_DAG
--              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) p)
--              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 8) p)

----------------------------------------------------------------------------------------------------------------
---- | Function imports from db_odbc.c

--foreign import ccall unsafe "hiredis/as_db.c getCells" c_getCells :: Ptr CString -> CInt -> IO (Ptr C_ASCells)
--foreign import ccall unsafe "hiredis/as_db.c setCells" c_setCells :: Ptr CString -> Ptr CString -> CInt -> IO ()
--foreign import ccall unsafe "hiredis/as_db.c deleteEdges" c_deleteEdges :: Ptr CString -> CInt -> IO ()
--foreign import ccall unsafe "hiredis/as_db.c insertEdges" c_insertEdges :: Ptr CString -> Ptr CString -> CInt -> IO ()
--foreign import ccall unsafe "hiredis/as_db.c getEdges" c_getEdges :: IO (Ptr C_ASDAG)
--foreign import ccall unsafe "hiredis/as_db.c pushCommit" c_pushCommit :: CString ->  IO () 
--foreign import ccall unsafe "hiredis/as_db.c undo" c_undo :: IO CString
--foreign import ccall unsafe "hiredis/as_db.c redo" c_redo :: IO CString
--foreign import ccall unsafe "hiredis/as_db/c clear" c_clear :: IO ()


----------------------------------------------------------------------------------------------------------------
---- | Conversion functions / helpers

--convertCell :: C_ASCell -> IO (ASCell)
--convertCell c = do 
--  l <- peekCString (location c)
--  ev <- peekCString (expVal c) 
--  let asEv = read ev :: ExpVal
--  return $ Cell (read l :: ASLocation) (redisExp asEv) (redisVal asEv)

--convertRelation :: C_ASRelation -> IO (ASLocation,ASLocation)
--convertRelation (C_Relation a b) = do 
--  s1 <- peekCString a
--  s2 <- peekCString b
--  return ((read s1 :: ASLocation),(read s2 :: ASLocation))

---- | Converts Haskell array of strings to C-ptr
--getPtr :: [String] -> IO (Ptr CString)
--getPtr lst = do 
--  cstrings <- mapM newCString lst
--  newArray cstrings

----------------------------------------------------------------------------------------------------------------
---- | DB ASCell functions

--deleteCell :: ASLocation -> IO ()
--deleteCell loc = deleteCells [loc]

--deleteCells :: [ASLocation] -> IO ()
--deleteCells locs = return ()

--getCells :: [ASLocation] -> IO [ASCell]
--getCells [] = return []
--getCells locs = do
--  putStrLn $ "locs in get cells: " L.++ (show locs)
--  l <- getPtr (L.map show locs)
--  putStrLn $ "got cell ptr: " L.++ (show l)
--  ptrCells <- c_getCells l (fromIntegral (L.length locs))
--  putStrLn $ "back to haskell from c in get cells: " 
--  ccells <- peek ptrCells
--  arrCells <- peekArray (fromIntegral (numCells ccells)) (cells ccells)
--  _ <- free l
--  _ <- free (cells ccells)
--  _ <- free ptrCells
--  res <- mapM convertCell arrCells
--  putStrLn $ "Get Cells: " L.++ (show res)
--  return res

--setCell :: ASCell -> IO ()
--setCell c = setCells [c]

--setCells :: [ASCell] -> IO ()
--setCells cells = do 
--  locs <- getPtr $ L.map show (L.map cellLocation cells)
--  expVals <- getPtr $ L.map show (L.map (\c -> (ExpVal (cellExpression c) (cellValue c))) cells)
--  c_setCells locs expVals (fromIntegral (L.length cells))
--  _ <- free locs
--  free expVals


----------------------------------------------------------------------------------------------------------------
---- | DB DAG functions


--getDAG :: IO [(ASLocation,ASLocation)]
--getDAG = do 
--  ptrDAG <- c_getEdges
--  putStrLn $ "back in Haskell after getting all edges"
--  d <- peek ptrDAG
--  putStrLn $ "num edges in dag " L.++ show (fromIntegral (numEdges d))
--  arrEdges <- peekArray (fromIntegral (numEdges d)) (dag d)
--  rels <- mapM convertRelation arrEdges
--  _ <- free (dag d)
--  _ <- free ptrDAG
--  putStrLn $ "DAG Edges: " L.++ (show rels)
--  return rels

--updateDAG :: [([ASLocation],ASLocation)] -> IO ()
--updateDAG update = do 
--  _ <- deleteDAGEdges (L.map snd update)
--  putStrLn $ "deletion works"
--  let edgeList = L.concat (L.map (\(a,b) -> zip a (L.repeat b)) update)
--  insertDAGEdges (L.map fst edgeList) (L.map snd edgeList)

--deleteDAGEdges :: [ASLocation] -> IO ()
--deleteDAGEdges toLocs = do 
--  tl <- getPtr (L.map show toLocs)
--  (c_deleteEdges tl (fromIntegral (L.length toLocs))) >> (free tl)

--insertDAGEdges :: [ASLocation] -> [ASLocation] -> IO ()
--insertDAGEdges fromLocs toLocs = do 
--  tl <- getPtr (L.map show toLocs)
--  fl <- getPtr (L.map show fromLocs)
--  (c_insertEdges fl tl (fromIntegral (L.length toLocs))) >> (free tl) >> (free fl)


----------------------------------------------------------------------------------------------------------------
---- Commit functions

--pushCommit :: ASCommit -> IO ()
--pushCommit commit = do 
--  c <- newCString (show commit)
--  _ <- c_pushCommit c
--  free c

---- hsc2hs DB.hsc -L -lodbc (ghc comes with hsc2hs)