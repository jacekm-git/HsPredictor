module TestSuites.LoadCSVSpec (spec) where

--standard
import Control.Exception (bracket, catch, throwIO, )
import Prelude hiding (catch)
import System.Directory (removeFile)
import System.IO.Error 
--3rd party
import Test.Hspec.Contrib.HUnit(fromHUnitTest)
import Test.HUnit
--own
import LoadCSV
import Types


spec = fromHUnitTest $ TestList [
  TestLabel ">>stats" test_stats,
  TestLabel ">>teams"  test_teams,
  TestLabel ">>resultsUpcoming"  test_resultsUpcoming,
  TestLabel ">>resultsAll"  test_resultsAll,
  TestLabel ">>reloadCSV" test_reloadCSV
  ]
                
testFilePath = "tests/tmp/test.csv"
testDbPath = "tests/tmp/test.db"

setUp = do
  removeIfExists testDbPath
  loadCSV testFilePath testDbPath
  
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

test_stats = TestCase $ do
  setUp
  teams <- getTeams testDbPath
  s1 <- filterStats "Blue" teams
  s2 <- filterStats "Red" teams
  assertEqual "Blue stats" s1 [4,4,4]
  assertEqual "Red stats" s2 [2,2,2]
  where
    filterStats t teams = flip getStats testDbPath
                          ((filter (\x -> x==t) teams)!!0)

test_teams = TestCase $ do
  setUp
  teams <- getTeams testDbPath
  assertEqual "Num of teams" (length teams) (3)

test_resultsAll = TestCase $ do
  setUp
  r <- getResultsAll testDbPath
  assertEqual "results all" (length r) (14)

test_resultsUpcoming = TestCase $ do
  setUp
  r <- getResultsUpcoming testDbPath
  assertEqual "result upcomings" (length r) (2)  
  
test_reloadCSV = TestCase $ do
  setUp
  r <- getResultsAll testDbPath
  assertEqual "results all length" (length r) (14)
  loadCSV testFilePath testDbPath -- load again
  r1 <- getResultsAll testDbPath
  assertEqual "results length after reload" (length r1) (14)
  assertEqual "results before and after" r r1
