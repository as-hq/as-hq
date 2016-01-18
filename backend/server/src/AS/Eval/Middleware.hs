module AS.Eval.Middleware (evalMiddleware) where

import Prelude()
import AS.Prelude

import Data.List
import Database.Redis (Connection)
import Data.Maybe

import AS.Types.Cell
import Safe (headMay)

import Control.Lens 

import AS.Eval.Core as R
import AS.Util as U
import AS.DB.API as DB
import AS.DB.Internal as DI

-- | This is middleware for evaluation; we take a cell recieved with the "Evaluate" action tag and preprocess it
-- Add tags 
evalMiddleware :: [ASCell] -> IO [ASCell]
evalMiddleware = return . cleanPossiblyExtaneousEqualSignsFromCells

cleanPossiblyExtaneousEqualSignsFromCells :: [ASCell] -> [ASCell]
cleanPossiblyExtaneousEqualSignsFromCells = map $ cellExpression %~ cleanPossiblyExtaneousEqualSign

-- | If the first char in your expression is an =, and your language isn't Excel, you
-- *probably* didn't intend to put it there. 
cleanPossiblyExtaneousEqualSign :: ASExpression -> ASExpression
cleanPossiblyExtaneousEqualSign xp = 
  case xp^.language of 
    Excel     -> xp
    otherwise -> case headMay $ xp^.expression of 
      Just '='  -> xp & expression .~ (tail $ xp^.expression)
      otherwise -> xp

-- addBackTags :: [ASCell] -> IO [ASCell]
-- addBackTags cells = do 
--   mOldCells <- DB.getCells (mapCellLocation cells)
--   return $ (flip map) (zip cells mOldCells) $ \(c, mc) -> case mc of 
--     Nothing -> c
--     Just c' -> c { cellTags = (cellTags c) `union` filteredOldTags }
--       where filteredOldTags = filter (not . isListTag) (cellTags c')

-- -- This would be much better to implement if we didn't store this as a tag...
-- -- #needsrefactor (Alex 11/11)
-- isListTag :: CellProp -> Bool
-- isListTag (ListMember _) = True
-- isListTag DFMember = True
-- isListTag _ = False

----------------------------------------------------------------------------------------------------------------------------------------------
-- Middlewares

-- | Change the excel expression to a python one and also possibly add a volatile tag
--evalInitExcel :: ASCell -> IO ASCell
--evalInitExcel c@(Cell loc xp@(Expression rawXp Excel) val ts) = do
--    initResult <- R.evalExcel xp
--    return $ case initResult of 
--        (Left valueE) -> Cell loc xp valueE ts
--        (Right (newXp, isVolatile)) -> case isVolatile of
--            False -> Cell loc newXp val ts
--            True -> case (U.hasVolatileTag c) of 
--                True -> Cell loc newXp val ts
--                False -> Cell loc newXp val (Volatile:ts)
--evalInitExcel cell = return cell

evalConnector :: ASCell -> IO ASCell
evalConnector cell = return cell -- TODO