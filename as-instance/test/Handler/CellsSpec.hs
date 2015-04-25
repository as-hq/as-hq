module Handler.CellsSpec (spec) where

import TestImport
import Data.Aeson

spec :: Spec
spec = withApp $ do
  it "puts a list into a single cell" do
    postBody CellsR $ encode $ Cell (Index (1, 1)) (Expression "[1,2,3]") (ValueNaN ())
    statusIs 200

