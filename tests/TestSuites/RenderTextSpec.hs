module TestSuites.RenderTextSpec (spec) where

import Test.Hspec.Contrib.HUnit(fromHUnitTest)
import Test.HUnit
import Test.Hspec               (hspec)

import HsPredictor.Render.Text
import HsPredictor.SQL.Queries
import HsPredictor.CSV.Load
import HelperFuncs (removeIfExists)

main = hspec spec

spec = fromHUnitTest $ TestList [
  TestLabel ">>renderList" test_renderList,
  TestLabel ">>convertList" test_convertList,
  TestLabel ">>renderTable" test_renderTable
  ]

stats = [("A", [3, 2 ,1]), ("B", [2, 3, 1])]

l = [["A", "1", "2", "3"]]

test_renderList = TestCase $ do
  let t = renderList l 3
  t @?= ["|  A|  1|  2|  3|", "-----------------"]

test_convertList = TestCase $ do
  let t = convertList stats
  t @?= [["A", "3", "2", "1"],
         ["B", "2", "3", "1"]]

testFilePath = "tests/tmp/test.csv"
testDbPath = "tests/tmp/test.db"

setUp = do
  removeIfExists testDbPath
  loadCSV testFilePath testDbPath

test_renderTable = TestCase $ do
  setUp
  t <- renderTable testDbPath 1
  t @?= (unlines ["|A|4|2|0|",
                  "---------",
                  "|C|2|3|1|",
                  "---------",
                  "|D|1|3|2|",
                  "---------",
                  "|B|0|2|4|",
                  "---------"])
