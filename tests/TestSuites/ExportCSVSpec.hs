{-# Language BangPatterns #-}
module TestSuites.ExportCSVSpec (spec) where

--standard
import System.IO (openFile, hGetContents,
                  hClose, IOMode(ReadMode))
-- 3rd party
import Test.Hspec.Contrib.HUnit(fromHUnitTest)
import Test.Hspec (hspec)
import Test.HUnit
-- own
import HelperFuncs (removeIfExists)
import LoadCSV (getFileContents, loadCSV)
import ExportCSV
import Parser (readMatches)
import Types

main = hspec $ spec

spec = fromHUnitTest $ TestList [
  TestLabel ">>genListsByDate" test_genListsByDate,
  TestLabel ">>getScaledWin" test_getScaledWin,
  TestLabel ">>getScaledDraw" test_getScaledDraw,
  TestLabel ">>getScaledLoss" test_getScaledLoss,
  TestLabel ">>normalize" test_normalize,
  TestLabel ">>prepareLine" test_prepareLine,
  TestLabel ">>export" test_export
  ]

testFilePath = "tests/tmp/test.csv"
testExportPath = "tests/tmp/test.exp"
testDbPath = "tests/tmp/test.db"
testDbPath3 = "tests/tmp/test3.db"

setUp = do
  removeIfExists testDbPath
  loadCSV testFilePath testDbPath

setUpExport = do
  removeIfExists testExportPath
  removeIfExists testDbPath3

  
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
    
test_getScaledStats = TestCase $ do
  setUp
  b <- getScaledStats testDbPath "Blue"
  g <- getScaledStats testDbPath "Blue"
  r <- getScaledStats testDbPath "Blue"
  b @?= Scaled 1 1 1
  g @?= Scaled (-1) (-1) (-1)
  r @?= Scaled (-1) (-1) (-1)

test_getScaledWin = TestCase $ do
  setUp
  winScaled <- getScaledWin testDbPath "Blue"
  winScaled @?= 1

test_getScaledDraw = TestCase $ do
  setUp
  drawScaled <- getScaledDraw testDbPath "Red"
  drawScaled @?= (-1)

test_getScaledLoss = TestCase $ do
  setUp
  lossScaled <- getScaledLoss testDbPath "Green"
  lossScaled @?= (-1)

test_normalize = TestCase $ do
  normalize 0 0 0 @?= (-1)
  normalize 1 1 1 @?= (-1)
  normalize 10 0 10 @?= 1
  normalize 0 0 10 @?= (-1)
  normalize 5 0 10 @?= 0
  normalize 3 0 10 @?= (-0.4)
  normalize 9 0 10 @?= 0.8

test_prepareLine = TestCase $ do
  setUp
  a <- prepareLine testDbPath "Blue" "Green" HomeWin
  b <- prepareLine testDbPath "Green" "Blue" NoWinner
  c <- prepareLine testDbPath "Blue" "Green" AwayWin
  a @?= "1.0 1.0 1.0 -1.0 -1.0 -1.0 \n-1\n"
  b @?= "-1.0 -1.0 -1.0 1.0 1.0 1.0 \n0\n"
  c @?= "1.0 1.0 1.0 -1.0 -1.0 -1.0 \n1\n"

test_export = TestCase $ do
  setUpExport
  export testDbPath3 testExportPath testFilePath
  exp <- getFileContents testExportPath
  let exp_list = lines exp
  length exp_list @?= 24
  exp_list !! 0 @?= "-1.0 -1.0 -1.0 -1.0 -1.0 -1.0 "
  exp_list !! 1 @?= "-1"
  exp_list !! 22 @?= "-1.0 -1.0 -1.0 1.0 1.0 1.0 "
  exp_list !! 23 @?= "1"
