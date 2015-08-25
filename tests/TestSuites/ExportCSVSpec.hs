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
import HsPredictor.LoadCSV (getFileContents, loadCSV)
import HsPredictor.ExportCSV
import HsPredictor.ParserCSV (readMatches)
import HsPredictor.Types

main = hspec $ spec

spec = fromHUnitTest $ TestList [
  TestLabel ">>getScaledStats" test_getScaledStats,
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
  a <- getScaledStats testDbPath "A"
  b <- getScaledStats testDbPath "B"
  d <- getScaledStats testDbPath "D"
  a @?= Scaled 1 (-1) (-1)
  b @?= Scaled (-1) (-1) 1
  d @?= Scaled (-0.5) 1 0

test_getScaledWin = TestCase $ do
  setUp
  a <- getScaledWin testDbPath "A"
  d <- getScaledWin testDbPath "D"
  a @?= 1
  d @?= (-0.5)

test_getScaledDraw = TestCase $ do
  setUp
  a <- getScaledDraw testDbPath "A"
  d <- getScaledDraw testDbPath "D"
  a @?= (-1)
  d @?= (1)

test_getScaledLoss = TestCase $ do
  setUp
  a <- getScaledLoss testDbPath "A"
  d <- getScaledLoss testDbPath "D"
  a @?= (-1)
  d @?= 0
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
  a <- prepareLine testDbPath "A" "B" HomeWin
  b <- prepareLine testDbPath "B" "A" NoWinner
  c <- prepareLine testDbPath "A" "D" AwayWin
  a @?= "1.0 -1.0 -1.0 -1.0 -1.0 1.0 \n-1\n"
  b @?= "-1.0 -1.0 1.0 1.0 -1.0 -1.0 \n0\n"
  c @?= "1.0 -1.0 -1.0 -0.5 1.0 0.0 \n1\n"

test_export = TestCase $ do
  setUpExport
  export testDbPath3 testExportPath testFilePath
  exp <- getFileContents testExportPath
  let exp_list = lines exp
  length exp_list @?= 24
  exp_list !! 0 @?= "-1.0 -1.0 -1.0 -1.0 -1.0 -1.0 "
  exp_list !! 1 @?= "-1"

