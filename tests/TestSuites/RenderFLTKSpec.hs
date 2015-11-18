module TestSuites.RenderFLTKSpec (spec) where

import Test.Hspec.Contrib.HUnit(fromHUnitTest)
import Test.HUnit
import Test.Hspec               (hspec)

import HsPredictor.Render.FLTK
import HsPredictor.SQL.Queries
import HsPredictor.CSV.Load
import HelperFuncs (removeIfExists)

main = hspec spec

spec = fromHUnitTest $ TestList [
  TestLabel ">>getTableData" test_getTableData
  ]

testFilePath = "tests/tmp/test.csv"
testDbPath = "tests/tmp/test.db"

setUp = do
  removeIfExists testDbPath
  loadCSV testFilePath testDbPath

test_getTableData = TestCase $ do
  setUp
  t <- getTableData testDbPath
  t @?= [["A", "4", "2", "0"],
         ["C", "2", "3", "1"],
         ["D", "1", "3", "2"],
         ["B", "0", "2", "4"]]
