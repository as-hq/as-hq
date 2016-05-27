{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AS.Prelude ( 
    module PreludeMinus
  , module LocatedPartials
  , module Data.Data
  , module GHC.Generics
  , module System.IO
  , module Control.Lens
  , module Control.Lens.TH
  , module Control.Applicative
  , module Control.Monad
  , module Control.Monad.Trans.Either
  , module Data.Maybe
  , module Data.Text
  , valAt
  , readEither
  , headMay
  , headMaybes
  , nub
  , filterBy
  , (<++>)
  , modifyMVar_'
  , catchAny
  , handleAny
  , handleIgnored
  , lift
  , liftIO
  , showConstructor
  , forkIO_
  , forkProcess'
  , parMapM_
  , parForM_
  , runEitherT_
  , unsafeRunEitherT
  , sequenceWith
  , whenLeft
  , whenJust
  , maybeM
  , getUUID
  , allEqual
  , isRectangular
  , for
  , catLines
  , threadDelaySeconds
  , when
  , none
  ) where

-- NOTE: THIS FILE SHOULD BE AN IMPORT ROOT!!

import Prelude as PreludeMinus hiding (
    head
  , tail
  , last
  , error
  , undefined
  , log
  , maximum
  , minimum
  , foldr1
  , foldl1
  , init
  , (!!)
  , read
  , cycle
  , get
  , empty
  )

import GHC.Err.Located        as LocatedPartials
import Data.Either.Located    as LocatedPartials
import Data.List.Located      as LocatedPartials (
    head
  , tail
  , last
  , init
  , foldr1
  , foldl1
  , foldl1'
  , foldl'
  , maximum
  , minimum
  , cycle
  , (!!)
  , splitAt
  , break
  , intercalate
  , isPrefixOf
  , drop
  , filter
  , reverse
  , replicate
  , take
  , sortBy
  , sort
  , intersperse
  , transpose
  , subsequences
  , permutations
  , partition
  , scanl
  , scanr
  , iterate
  , repeat
  , unfoldr
  , takeWhile
  , dropWhile
  , group
  , inits
  , tails
  , zipWith
  , zip
  , find
  , intercalate
  , findIndex
  , dropWhile
  , dropWhileEnd
  , groupBy
  , union
  )

import Data.Maybe (
    catMaybes
  , isNothing
  , mapMaybe
  )

import Data.Text (Text)

import Control.Concurrent
import Control.Exception
import Control.Lens.TH
import Control.Lens hiding (
    from
  , to
  , Iso
  , (.=)
  , (.>)
  , index
  , Context
  , transform
  , Setter
  , Getter
  , set
  , get
  , none
  )
import Data.Maybe.Located     as LocatedPartials
import Text.Read.Located      as LocatedPartials (read)

import Data.Data

import Control.Applicative
import Control.Concurrent
import Control.Exception

import Language.Haskell.TH

import Safe (headMay, lastMay, tailMay)
import Text.Read (readEither)

import qualified Data.Map as M
import Control.Applicative

import System.IO
import GHC.Generics hiding (
    Prefix
  , Infix
  , Fixity
  , R
  , prec
  )

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Concurrent       (threadDelay)
import Control.Monad hiding (
  when
  )
import qualified Control.Monad as Monad

import qualified Control.Monad.Catch as MC
import Control.Monad.IO.Class
import Control.Monad.Trans.Either 

import System.Posix.Types (ProcessID)
import System.Posix.Process (forkProcess, exitImmediately)
import System.Exit (ExitCode(..))

import Data.UUID.V1 (nextUUID)
import Data.UUID    (toString)

import GHC.Stack

-------------------------------------------------------------------------------------------------------------------------
-- safe error-reporting functions

-- | Safe unsafe version of lookup; think of it as fromJust . lookup
valAt :: (?loc :: CallStack) => (Ord a) => a -> M.Map a b -> b
valAt key mp = case key `M.lookup` mp of 
  Nothing -> error "key not found in map" 
  Just h  -> h

-------------------------------------------------------------------------------------------------------------------------
-- error handling

-- | A version of catch that never swallows asynchronous exceptions. Works
-- not only for IO.
catchAny :: (MC.MonadCatch m, MonadIO m) => 
            m a -> -- initial IO/monadic action
            (SomeException -> m a) -> -- error IO/monadic action
            m a
catchAny m f = MC.catch m onExc
  where
    onExc e
        | shouldCatch e = f e
        | otherwise = throw e
    shouldCatch e
        | Just (_ :: AsyncException) <- fromException e = False
        | otherwise = True

-- | a version of handle that never swallows asynchronous exceptions.
handleAny :: (MC.MonadCatch m, MonadIO m) => (SomeException -> m a) -> m a -> m a
handleAny h f = catchAny f h

handleIgnored :: (MC.MonadCatch m, MonadIO m) => m a -> m ()
handleIgnored f = handleAny (const $ return ()) (void f)

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

showConstructor :: (Data a) => a -> String
showConstructor = showConstr . toConstr

forkIO_ :: IO a -> IO ()
forkIO_ = void . forkIO . void

-- | Prevents GC stats (& other RTS info) from being printed when the process dies
forkProcess' :: IO () -> IO ProcessID
forkProcess' f =  
  forkProcess $ f >> exitImmediately ExitSuccess

runEitherT_ :: EitherT a IO b -> IO ()
runEitherT_ = void . runEitherT 

unsafeRunEitherT :: EitherT a IO b -> IO b
unsafeRunEitherT x = fromRight <$> runEitherT x 

sequenceWith :: (Monad m) => (a -> b) -> [m a] -> m [b]
sequenceWith f = sequence . map (f <$>)

whenLeft :: (Monad m) => Either a b -> m c -> m ()
whenLeft (Right _) _ = return ()
whenLeft (Left _)  f = void f

whenJust :: (Monad m) => Maybe a -> (a -> m b) -> m ()
whenJust Nothing  _ = return ()
whenJust (Just x) f = void $ f x

parMapM_ :: (a -> IO ()) -> [a] -> IO ()
parMapM_ f xs = mapM_ (forkIO_ . f) xs

parForM_ :: [a] -> (a -> IO ()) -> IO ()
parForM_ = flip parMapM_

-- | a hyphenated alphanumeric UUID 
getUUID :: IO String
getUUID = do
  u <- nextUUID
  case u of 
    Just u  -> return $ toString u
    -- according to https://hackage.haskell.org/package/uuid-1.3.12/docs/Data-UUID-V1.html,
    -- `nextUUID` returns Nothing if you request UUIDs too quickly.
    Nothing -> putStrLn "could not generate UUID!" >> threadDelay 100 >> getUUID

maybeM :: (Monad m) => m (Maybe a) -> m b -> (a -> m b) -> m b
maybeM m c k = m >>= maybe c k

headMaybes :: [Maybe a] -> Maybe a
headMaybes xs = case xs of 
  [x] -> x
  _   -> Nothing

-- | Check if all elements of a list are the same
allEqual :: (Eq a) => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = and $ map (== x) xs

-- | Check if a list of lists is rectangular
isRectangular :: [[a]] -> Bool
isRectangular lst = allEqual lenRows
  where
    lenRows = map length lst

for :: [a] -> (a -> b) -> [b]
for = flip map

catLines :: String -> String
catLines = foldl' (++) "" . lines

threadDelaySeconds :: Int -> IO ()
threadDelaySeconds = threadDelay . (1000000 *)

when :: (Monad m) => Bool -> m a -> m ()
when c = Monad.when c . void

none :: (a -> Bool) -> [a] -> Bool
none f xs = not $ any f xs