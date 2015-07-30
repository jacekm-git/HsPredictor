module TestSuites.ExportCSVSpec (spec) where

import Test.Hspec.Contrib.HUnit(fromHUnitTest)
import Test.HUnit
import ExportCSV
import Parser (readMatches)
import Types

spec = fromHUnitTest $ TestList [
  TestLabel ">>genListsByDate" test_genListsByDate,
  TestLabel ">>getScaledWin" test_getScaledWin,
  TestLabel ">>getScaledDraw" test_getScaledDraw,
  TestLabel ">>getScaledLoss" test_getScaledLoss,
  TestLabel ">>normalize" test_normalize
  ]

testFilePath = "tests/tmp/test.csv"
testDbPath = "tests/tmp/test.db"



test_genListsByDate = TestCase $ do
  let test_list = [Match 20000101 "Home" "Away" 1 1 1 2 3,
                   Match 20000101 "Home" "Away" 2 1 1 2 3,
                   Match 20000101 "Home" "Away" 3 1 1 2 3,
                   Match 20000102 "Home" "Away" 4 1 1 2 3,
                   Match 20000103 "Home" "Away" 5 1 1 2 3]

  let gen_list = [[Match 20000101 "Home" "Away" 1 1 1 2 3,
                   Match 20000101 "Home" "Away" 2 1 1 2 3,
                   Match 20000101 "Home" "Away" 3 1 1 2 3],
                  [Match 20000102 "Home" "Away" 4 1 1 2 3],
                  [Match 20000103 "Home" "Away" 5 1 1 2 3]]
  (genListsByDate test_list) @?= gen_list
    

test_getScaledWin = TestCase $ do
  winScaled <- getScaledWin testDbPath "Blue"
  winScaled @?= (1)

test_getScaledDraw = TestCase $ do
  drawScaled <- getScaledDraw testDbPath "Red"
  drawScaled @?= (-1)

test_getScaledLoss = TestCase $ do
  lossScaled <- getScaledLoss testDbPath "Green"
  lossScaled @?= (-1)

test_normalize = TestCase $ do
  normalize 0 0 0 @?= (-1)
  normalize 1 1 1 @?= (-1)
  normalize 10 0 10 @?= (1)
  normalize 0 0 10 @?= (-1)
  normalize 5 0 10 @?= (0)
  normalize 3 0 10 @?= (-0.4)
  normalize 9 0 10 @?= (0.8)
