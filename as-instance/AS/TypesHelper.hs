module AS.TypesHelper where 

import AS.Types
import Import hiding (index)
import Prelude ((!!))
import Text.ParserCombinators.Parsec
import Data.Char


sameCol :: ASCell -> ASCell -> Bool
sameCol cell1 cell2 = (fst (index (cellLocation cell1))) == (fst (index (cellLocation cell2)))

sameRow :: ASCell -> ASCell -> Bool
sameRow cell1 cell2  = (snd (index (cellLocation cell1))) == (snd (index (cellLocation cell2)))

