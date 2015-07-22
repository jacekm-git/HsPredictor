module TestSuites.LoadCSV (tests_LoadCSV) where

--standard
import Control.Exception (bracket, catch, throwIO, )
import Prelude hiding (catch)
import System.Directory (removeFile)
import System.IO.Error 
--3rd party
import Test.HUnit
--own
import LoadCSV
import Types


tests_LoadCSV = TestList [
  test_stats,
  test_teams,
  test_resultsUpcoming,
  test_resultsAll
  ]
                
testFilePath = "tests/tmp/test_file.csv"
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
  s1 <- filterStats "Freiburg" teams
  s2 <- filterStats "Leverkusen" teams
  assertEqual "Freiburg stats" s1 [14,9,11]
  assertEqual "Leverkusen stats" s2 [19,8,7]
  where
    filterStats t teams = flip getStats testDbPath
                          ((filter (\x -> x==t) teams)!!0)

test_teams = TestCase $ do
  setUp
  teams <- getTeams testDbPath
  assertEqual "Num of teams" (length teams) (18)

test_resultsAll = TestCase $ do
  setUp
  r <- getResultsAll testDbPath
  assertEqual "results all" (length r) (307)

test_resultsUpcoming = TestCase $ do
  setUp
  r <- getResultsUpcoming testDbPath
  assertEqual "result upcomings" (length r) (1)  
  
