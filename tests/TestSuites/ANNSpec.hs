module TestSuites.ANNSpec(spec) where
--3rd party
import           System.Directory         (doesFileExist)
import           Test.Hspec               (hspec)
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit
--own
import           HelperFuncs              (removeIfExists)
import           HsPredictor.ANN
import           HsPredictor.ExportCSV

main = hspec spec
spec = fromHUnitTest $ TestList [
  TestLabel ">>trainAndSaveANN" test_trainAndSaveANN,
  TestLabel ">>useANN" test_useANN
  ]

expPath = "tests/tmp/test.exp"
annPath = "tests/tmp/test.ann"
csvPath = "tests/tmp/test.csv"
dbPath = "tests/tmp/test.db"

test_trainAndSaveANN = TestCase $ do
  removeIfExists annPath
  removeIfExists dbPath
  removeIfExists expPath
  export dbPath expPath csvPath
  trainAndSaveANN expPath annPath 1000 0 0.001
  exists <- doesFileExist annPath
  exists @?= True

test_useANN = TestCase $ do
  [result0] <- useANN annPath [1.0,-1.0,-1.0,-1.0,1.0,0.33333333333333326]
  [result1] <- useANN annPath [0.33333333333333326,1.0,-1.0,1.0,0.0,-1.0]
  assertBool "" $ if result0 < 0.1 then True else False
  assertBool "" $ if result1 > 0.9 then True else False
