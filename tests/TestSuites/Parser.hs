module TestSuites.Parser (tests_Parser) where

import Test.HUnit
import Parser
import Types

tests_Parser = TestList [
  test_readMatches
  ]

test_readMatches = TestCase $ do
  let r1 = ["2012.08.24,Dortmund,Bremen,2,3"]
  let r2 = ["20.08.24,Dortmund,Bremen,2,3"]
  let r3 = ["2012.08.24,Dortmund,Bremen,2,three"]
  let r4 = ["2012.08.24,Dortmund,Bremen,2"]
  let r5 = ["2012.08.24,Dortmund,Bremen,-1,-1"]
  let r6 = ["2012.08.24,Dortmund,Bremen,-1,1"]
  let r7 = ["2012.08.24,Dortmund,Bremen,1,-1"]
  assertEqual "Good input"
    [(20120824, "Dortmund" ,"Bremen", 2, 3 )]
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
    [(20120824, "Dortmund" ,"Bremen", -1, -1 )]
    (readMatches r5)
  assertEqual "Upcoming match bad1"
    []
    (readMatches r6)
  assertEqual "Upcoming match bad2"
    []
    (readMatches r7)

  

