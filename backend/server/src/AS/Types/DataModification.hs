{-# LANGUAGE RankNTypes #-}
module AS.Types.DataModification where

import Data.Data
import AS.Prelude
-- |  Applies the first function recursively to all values of type s
-- used to construct a.
--  Example:
--  let x = Cons 5 (Cons 4 Nil)
--  let y = recurseF ((+1) :: Int -> Int) x
--  --  y = Cons 6 (Cons 5 Nil)
deepGMapT :: (Typeable s) => (s -> s) -> ((Data a) => a -> a)
deepGMapT t x = case (t <$> cast x) of
             Just s -> $fromJust $ cast s
             Nothing -> gmapT (deepGMapT t) x

-- | Applies the first function recursively to all values of type s
-- used to construct a, now with error handling. The monad m is currently only
-- ever a Maybe.

--  Examples:
--  let f = (\d -> if d > 0 then Just (d+1) else Nothing)
--  let x = Cons 5 (Cons -1 Nil)
--  let y = deepGMapM (f :: Int -> Int) x
--  --  y = Nothing
--
--  let x = Cons 5 (Cons 4 Nil)
--  let y = deepGMapM (f :: Int -> Int) x
--  --  y = Just $ Cons 6 (Const 5 Nil) 
--
deepGMapM :: (Monad m, Data s) => (s -> m s) -> (Data a => a -> m a)
deepGMapM t x = case (t <$> cast x) of
             Just monadS -> ($fromJust . cast) <$> monadS
             Nothing -> gmapM (deepGMapM t) x

