import AS.Types.Core
import AS.Types.DB
import AS.Types.Excel
import AS.DB.API as DB
import AS.DB.Util as DU
import AS.Util

import AS.Kernels.Python.Eval as KP
import AS.Kernels.LanguageUtils
import AS.Kernels.Excel.Compiler as EC
import AS.Kernels.Excel.Eval as EE
import AS.Kernels.Excel.Lib as EL

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map as M

import Text.ParserCombinators.Parsec

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

import Test.Hspec hiding (context)
import System.IO.Unsafe

eval :: String -> Either ASExecError ASValue
eval s = unsafePerformIO $ runEitherT $ EE.evaluate s (IndexRef (Index (T.pack "") (1,1))) mp

mp :: RefValMap
mp = M.fromList $ [(IndexRef (Index (T.pack "") (i,j)), ValueI j)| i <- [1..100], j <- [1..100]]

tests :: [IO ()]
tests = [testLambda, testAdd,testAbs,testEquals,testIf,testSumIf]

testShitExpression :: IO ()
testShitExpression = hspec $ do
  describe "testShitExpression" $ do
    it "can parse anything without equals" $ do
      EL.getLambda (EValueS ">=2") (EValueNum (EValueD 5)) `shouldBe` True
      EL.getLambda (EValueS ">=2") (EValueNum (EValueD 1)) `shouldBe` False

testLambda :: IO ()
testLambda = hspec $ do
  describe "testLambda" $ do
    it "can do comparisons" $ do
      EL.getLambda (EValueS ">=2") (EValueNum (EValueD 5)) `shouldBe` True
      EL.getLambda (EValueS ">=2") (EValueNum (EValueD 1)) `shouldBe` False

testAdd :: IO ()
testAdd = hspec $ do
  describe "testAdd" $ do
    it "can add ints" $ do
      eval "=1+1" `shouldBe` (Right $ ValueI 2)

testAbs :: IO ()
testAbs = hspec $ do
  describe "testAbs" $ do
    it "can do abs" $ do
      eval "=abs(1)" `shouldBe` (Right $ ValueI 1)
    it "can scalarize" $ do
      eval "=abs(B1:B5)" `shouldBe` (Right $ ValueI 1)

testEquals :: IO ()
testEquals = hspec $ do
  describe "testEquals" $ do
    it "can get ints right" $ do
      eval "=A1=1" `shouldBe` (Right $ ValueB True)
    it "can do array formula" $ do
      eval "{=A1:A2=1}" `shouldBe` (Right $ ValueL [ValueB True, ValueB False])

testIf :: IO ()
testIf = hspec $ do
  describe "testIf" $ do
    it "can do if" $ do
      eval "=if(A1=1,2,3)" `shouldBe` (Right $ ValueI 2)
    it "can do array formula" $ do
      eval "{=if(A1:B1=1,B1:C1,55)}" `shouldBe` (Right $ ValueL [ValueL [ValueI 1], ValueL [ValueI 1]])
      eval "{=if(A1:A2=1,B1:C2,55)}" `shouldBe` (Right $ ValueL [ValueL [ValueI 1,ValueI 55], ValueL [ValueI 1,ValueI 55]])

testIfError :: IO ()
testIfError = hspec $ do
  describe "testIfError" $ do
    it "can do iferror" $ do
      eval "=iferror(1+\"hi\",5,6)" `shouldBe` (Right $ ValueI 5)
      eval "=iferror(A1,5,6)" `shouldBe` (Right $ ValueI 6)

testSumIf :: IO ()
testSumIf = hspec $ do
  describe "testSumIf" $ do
    it "can do basic filtering" $ do
      eval "=sumif(G1:I1,1,G1:I1)" `shouldBe` (Right $ ValueI 3)
    it "can do lambdas" $ do
      eval "=sumif(A1:A3,\">1\",B1:B3)" `shouldBe` (Right $ ValueI 5)

main :: IO ()
main = do
    putStrLn ""
    sequence tests
    putStrLn $ "Tests completed"


{-
     let context = (Context mp (IndexRef (Index (T.pack "") (1,1))))
        (Right (f,b)) = EC.parseFormula "{=if(A1:A2=2,B1:C2,55)}"
        refs = EL.getUnexpectedRefs (T.pack "") f
        dim = EL.getCommonDimension refs
        refMap = EL.unexpectedRefMap context refs
    -- putStrLn $ show refMap
    -- putStrLn $ show $ EL.refToEntity context $ ERef $ RangeRef $ Range (T.pack "") ((1,1),(1,2))
    putStrLn $ show $ EL.evalArrayFormula context f
-}
