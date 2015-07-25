module TestSuites.ParserSpec (spec) where

import Test.Hspec.Contrib.HUnit(fromHUnitTest)
import Test.HUnit
import Parser
import Types

spec = fromHUnitTest $ TestList [
  TestLabel ">>readMatches"  test_readMatches
  ]

test_readMatches = TestCase $ do
  let r1 = ["2012.08.24,Dortmund,Bremen,2,3,1.0,2.0,3.0"]
  let r2 = ["20.08.24,Dortmund,Bremen,2,3,-1,-1,-1"]
  let r3 = ["2012.08.24,Dortmund,Bremen,2,three,-1,-1,-1"]
  let r4 = ["2012.08.24,Dortmund,Bremen,2,-1,-1"]
  let r5 = ["2012.08.24,Dortmund,Bremen,-1,-1,1.0,2.0,3.0"]
  let r6 = ["2012.08.24,Dortmund,Bremen,-1,1,1.0,2.0,3.0"]
  let r7 = ["2012.08.24,Dortmund,Bremen,1,-1,-1,-1,-1"]
  assertEqual "Good input"
    [(20120824, "Dortmund" ,"Bremen", 2, 3, 1, 2, 3)]
    (readMatches r1)
  assertEqual "Wrong date format"
    []
    (readMatches r2)
  assertEqual "Wrong result format"
    []
    (readMatches r3)
  assertEqual "Not complete line"
    []
    (readMatches r4)
  assertEqual "Upcoming match good input"
    [(20120824, "Dortmund" ,"Bremen", -1, -1, 1.0, 2.0, 3.0 )]
    (readMatches r5)
  assertEqual "Upcoming match bad1"
    []
    (readMatches r6)
  assertEqual "Upcoming match bad2"
    []
    (readMatches r7)
