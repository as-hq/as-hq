module AS.Prelude 
  ( module PreludeMinus
  , error
  , read
  , head
  , fromJust
  , fromRight
  , nub
  , filterBy
  , (<++>)
  ) where

-- NOTE: THIS FILE SHOULD BE AN IMPORT ROOT!!

import Prelude as PreludeMinus hiding (head, read, error)
import qualified Prelude as P

import Control.Lens
import Language.Haskell.TH
import Safe (headMay)
import Text.Read (readEither)

import qualified Data.Map as M
import Control.Applicative

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
head' errorReporter l = case (headMay l) of 
  Nothing -> errorReporter "head got empty list!"
  Just h  -> h

read :: Q Exp
read = appE [|read'|] error

read' :: (Read a) => (String -> a) -> String -> a
read' errorReporter str = case (readEither str) of 
  Left e -> errorReporter $ "Read failed, because: " ++ e
  Right a -> a

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
