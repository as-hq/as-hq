{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AS.Prelude 
  ( module PreludeMinus
  , module Data.Data
  , module GHC.Generics
  , module System.IO
  , module Control.Monad
  , error
  , undefined
  , valAt
  , read
  , head
  , tail
  , last
  , fromJust
  , fromRight
  , nub
  , filterBy
  , (<++>)
  , modifyMVar_'
  , catchAny
  , handleAny
  , liftIO
  , showConstructor
  ) where

-- NOTE: THIS FILE SHOULD BE AN IMPORT ROOT!!

import Prelude as PreludeMinus hiding (head, tail, last, read, error, undefined, log)
import qualified Prelude as P
import Data.Data

import Control.Concurrent
import Control.Exception
import Control.Lens
import Language.Haskell.TH
import Safe (headMay, lastMay, tailMay)
import Text.Read (readEither)

import qualified Data.Map as M
import Control.Applicative

import System.IO
import GHC.Generics hiding (Prefix, Infix, Fixity, R)
import Control.Monad.IO.Class (liftIO)
import Control.Monad

-------------------------------------------------------------------------------------------------------------------------
-- error with locations

error :: Q Exp 
error = locatedError =<< location

locatedError :: Loc -> Q Exp
locatedError loc = [|(\msg -> P.error ("\x1b[1;31mError\x1b[0m at " ++ $(litE $ stringL prefix) ++ msg))|]
  where
    prefix = formattedLoc ++ " ----> "
    formattedLoc = concat [file, ":", show line, ":", show col]
    file = loc_filename loc
    (line, col) = loc_start loc

-------------------------------------------------------------------------------------------------------------------------
-- safe error-reporting functions

-- | Safe unsafe version of lookup; think of it as $fromJust . lookup
valAt :: Q Exp
valAt = appE [|valAt'|] error

valAt' :: (Ord a) => (String -> b) -> a -> M.Map a b -> b
valAt' errorReporter key mp = case key `M.lookup` mp of 
  Nothing -> errorReporter $ "key not found in map" 
  Just h  -> h

fromJust :: Q Exp
fromJust = appE [|fromJust'|] error

-- note: I can't put these helpers in 'where' clauses, because
-- template haskell doesn't give them a lifted instance then
fromJust' :: (String -> a) -> Maybe a -> a
fromJust' errorReporter Nothing = errorReporter "fromJust got Nothing!"
fromJust' _ (Just x) = x

fromRight :: Q Exp
fromRight = appE [|fromRight'|] error

fromRight' :: (String -> r) -> Either l r -> r
fromRight' errorReporter (Left _) = errorReporter "fromRight got Left!"
fromRight' _ (Right r) = r

head :: Q Exp
head = appE [|head'|] error

head' :: (String -> a) -> [a] -> a
head' errorReporter l = case headMay l of 
  Nothing -> errorReporter "head got empty list!"
  Just h  -> h

tail :: Q Exp
tail = appE [|tail'|] error

tail' :: (String -> [a]) -> [a] -> [a]
tail' errorReporter l = case tailMay l of 
  Nothing -> errorReporter "tail got empty list!"
  Just t  -> t

last :: Q Exp
last = appE [|last'|] error

last' :: (String -> a) -> [a] -> a
last' errorReporter l = case lastMay l of 
  Nothing -> errorReporter "last got empty list!"
  Just h  -> h

read :: Q Exp
read = appE [|read'|] error

read' :: (Read a) => (String -> a) -> String -> a
read' errorReporter str = case readEither str of 
  Left e -> errorReporter $ "Read failed, because: " ++ e
  Right a -> a

undefined :: Q Exp
undefined = appE error [|"Undefined!"|]

--get :: Q Exp
--get = appE [|get'|] error

--infix 9 get'
--get' :: (Ord k, Show k, Show a) => (String -> a) -> M.Map k a -> k -> a
--get' errorReporter mp key = case M.lookup key mp of
--  Nothing -> errorReporter $ "Map lookup (get) failed because the key " ++ (show key) ++ "was not found in the map: " ++ (show map)
--  Just x -> x

-------------------------------------------------------------------------------------------------------------------------
-- other standard functions we want

nub :: (Eq a, Ord a) => [a] -> [a]
nub xs = map fst $ M.toList . M.fromList $ zip xs (repeat ())

(<++>) a b = (++) <$> a <*> b

filterBy :: (a -> b) -> (b -> Bool) -> [a] -> [a]
filterBy f filt l = map snd $ filter (\(fe, _) -> filt fe) $ zip (map f l) l

-- | a version of modifyMVar_ that strictly computes the new state
modifyMVar_' :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_' s f = modifyMVar s $ \s' -> (,()) <$> f s'

-- | a version of catch that never swallows asynchronous exceptions.
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny m f = catch m onExc
  where
    onExc e
        | shouldCatch e = f e
        | otherwise = throwIO e
    shouldCatch e
        | Just (_ :: AsyncException) <- fromException e = False
        | otherwise = True

-- | a version of handle that never swallows asynchronous exceptions.
handleAny :: (SomeException -> IO a) -> IO a -> IO a
handleAny h f = catchAny f h

showConstructor :: (Data a) => a -> String
showConstructor = showConstr . toConstr