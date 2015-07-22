import System.Exit

import Test.HUnit
import TestSuites.HashCSV
import TestSuites.Parser
import TestSuites.LoadCSV


main :: IO ()
main = do
  cs@(Counts _ _ errs fails) <- runTestTT unitTests
  putStrLn (showCounts cs)
  if (errs > 0 || fails > 0)
    then exitFailure
    else exitSuccess

unitTests = TestList [
  TestLabel "HashCSV" tests_HashCSV
  , TestLabel "Parser" tests_Parser
  , TestLabel "LoadCSV" tests_LoadCSV
  ]

