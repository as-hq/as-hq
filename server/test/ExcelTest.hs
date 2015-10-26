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
mp = M.fromList $ [(IndexRef (Index (T.pack "") (i,j)), ValueI j)| i <- [1..10], j <- [1..10]]

tests :: [IO ()]
tests = [testLiteral, testLambda, testAdd, testAbs, testEquals, testIf, testSumIf, testCountIf]

testLiteral :: IO ()
testLiteral = hspec $ do
  describe "testLiteral" $ do
    it "can parse integers" $ do
      eval "123" `shouldBe` (Right $ ValueI 123)
    it "can parse doubles" $ do
      eval "1.23" `shouldBe` (Right $ ValueD 1.23)
    {-it "can parse strings" $ do
      eval "\"clifford_the_big_red_penis\"" `shouldBe` (Right $ (ValueS "clifford_the_big_red_penis"))
    it "can parse integers" $ do
      eval "\"s+_sdfja39w9df4wfm4=4524t2\"" `shouldBe` (Right $ (ValueS "s+_sdfja39w9df4wfm4=4524t2")) -}

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
    it "can resize" $ do
      eval "=sumif(A1:A4,\">=1\",B1:B3)" `shouldBe` (Right $ ValueI 10)
      eval "=sumif(A1:A2,\">=1\",B1:B3)" `shouldBe` (Right $ ValueI 3)

testCountIf :: IO ()
testCountIf = hspec $ do
  describe "testCountIf" $ do
    it "can do basic filtering" $ do
      eval "=countif(G1:I1,1)" `shouldBe` (Right $ ValueI 3)
    it "can do lambdas" $ do
      eval "=countif(A1:A3,\">1\")" `shouldBe` (Right $ ValueI 2)

testCountIfs :: IO ()
testCountIfs = hspec $ do
  describe "testCountIfs" $ do
    it "can do basic filtering" $ do
      eval "=countifs(G1:I1,1,G1:I1,0)" `shouldBe` (Right $ ValueI 0)
      eval "=countifs(G1:G3,2,H1:H3,2)" `shouldBe` (Right $ ValueI 1)

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
