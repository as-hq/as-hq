module Handler.ASSpec (spec) where

import Prelude as P
import Test.Hspec
import Test.Hspec.QuickCheck
import TestImport
import Data.Aeson
import AS.Types

spec :: Spec 
spec = do
  let stdloc = Index "Sheet-1" (1,1)
  specExpression "puts a list into a single cell" [(stdloc, (Expression "[1,2,3]" Python))] ["ValueD"]
  specExpression "check basic R comp" [(stdloc, (Expression "qt(.4,df=8)" R))] ["0.2619"]
  specExpression "python plots" [(stdloc, (Expression "plot([1,2],[3,4],\"test\")" Python))] ["ValueImage"]
  let cell1 = (stdloc, (Expression "66" Python))
  let cell2 = ((Index "Sheet-1" (2,1)), (Expression "A1+1" Python))
  specExpression "dependency" [cell1, cell2] ["ValueD", "67"]
  specExpression "ETF loading" [(stdloc, (Expression "ETF.loadSamples(4)" Python))] ["RDSA"]

specExpression :: String -> [(ASLocation, ASExpression)] -> [String]-> Spec
specExpression title lst asserts = withApp $ do -- in the YesodExample monad
  it title $ do
    P.mapM_ ( \(loc,xp) -> postBody CellsR $ encode $ Cell loc xp (ValueNaN ()) ) lst 
    printBody -- prints JSON to dist/test log
    P.mapM_ bodyContains asserts --check to see if JSON response contains assert
        -- could also use bodyEquals once we're confident in the "right" answers
    statusIs 200 -- did it work
    -- encode is from Data.Aeson.Encode, changes JSON to ByteString
    -- postBody is from Yesod.Test