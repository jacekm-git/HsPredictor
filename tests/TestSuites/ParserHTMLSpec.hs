module TestSuites.ParserHTMLSpec(spec, matches) where
--3rd party
import Test.Hspec.Contrib.HUnit(fromHUnitTest)
import Test.Hspec (hspec)
import Test.HUnit
--own
import ParserHTML

main = hspec spec

spec = fromHUnitTest $ TestList [
  TestLabel ">>readMatches" test_readMatches,
  TestLabel ">>convertHtml" test_convertHtml,
  TestLabel ">>fillHoles" test_fillHoles]

-- readMatches test data
matches = [
  "15.08.2014,Swansea - Newcastle Utd, 1.92, 3.52, 4.06",
  "30.08.2014, Everton - Chelsea, 3:6, 4.05, 3.42, 1.96",
  "31.08.2014, Everton - Chelsea, 3:6, 4.05",
  "32.08.2014,Everton - Chelsea, 3:6",
  "33.08.2014, Everton - Chelsea",
  "34.08.2014, Everton - Chelsea,"]
  
shouldBe = [
  "2014.08.15,Swansea,Newcastle Utd,-1,-1,1.92,3.52,4.06 \n",
  "2014.08.30,Everton,Chelsea,3,6,4.05,3.42,1.96 \n",
  "2014.08.31,Everton,Chelsea,3,6,-1.0,-1.0,-1.0 \n",
  "2014.08.32,Everton,Chelsea,3,6,-1.0,-1.0,-1.0 \n",
  "2014.08.33,Everton,Chelsea,-1,-1,-1.0,-1.0,-1.0 \n",
  "2014.08.34,Everton,Chelsea,-1,-1,-1.0,-1.0,-1.0 \n"]

-- convertHtml test data
resultsPath = "tests/tmp/results.html"
fixturesPath = "tests/tmp/fixtures.html"

fixturesTest = [
  "15.08.2015 13:45, Southampton - Everton, 1.94, 3.48, 4.10",
  "15.08.2015 16:00, Sunderland - Norwich, 2.64, 3.20, 2.81"]

fixturesMatchesTest = [
  "2015.08.15,Southampton,Everton,-1,-1,1.94,3.48,4.10 \n",
  "2015.08.15,Sunderland,Norwich,-1,-1,2.64,3.20,2.81 \n"]               

resultsTest = [
  "14.08.2015, Aston Villa - Manchester United, 0:1, 5.17, 3.63, 1.73",
  "10.08.2015, West Brom - Manchester City, 0:3, 5.80, 3.99, 1.61",
  "09.08.2015, Arsenal - West Ham, 0:2, 1.23, 6.32, 13.06",
  "09.08.2015, Newcastle Utd - Southampton, 2:2, 2.94, 3.22, 2.52"]

resultsMatchesTest = [
  "2015.08.14,Aston Villa,Manchester United,0,1,5.17,3.63,1.73 \n",
  "2015.08.10,West Brom,Manchester City,0,3,5.80,3.99,1.61 \n",
  "2015.08.09,Arsenal,West Ham,0,2,1.23,6.32,13.06 \n",
  "2015.08.09,Newcastle Utd,Southampton,2,2,2.94,3.22,2.52 \n"]

-- fillHoles testData
holesTestList = [
  "14.08.2015, Aston Villa - Manchester United, 0:1, 5.17, 3.63, 1.73",
  "West Brom - Manchester City, 0:3, 5.80, 3.99, 1.61",
  "09.08.2015, Arsenal - West Ham, 0:2, 1.23, 6.32, 13.06",
  "Newcastle Utd - Southampton, 2:2, 2.94, 3.22, 2.52"]

holesTestList2 = [
  "14.08.2015, Aston Villa - Manchester United, 0:1, 5.17, 3.63, 1.73",
  "14.08.2015,West Brom - Manchester City, 0:3, 5.80, 3.99, 1.61",
  "09.08.2015, Arsenal - West Ham, 0:2, 1.23, 6.32, 13.06",
  "09.08.2015,Newcastle Utd - Southampton, 2:2, 2.94, 3.22, 2.52"]


-- tests                 
test_readMatches = TestCase $ do
  let m = readMatches $ [matches !! 0]
  let m1 = readMatches $ [matches !! 1]
  let m2 = readMatches $ [matches !! 2]
  let m3 = readMatches $ [matches !! 3]
  let m4 = readMatches $ [matches !! 4]
  let m5 = readMatches $ [matches !! 5]
  m @?= [shouldBe !! 0]
  m1 @?= [shouldBe !! 1]
  m2 @?= [shouldBe !! 2]
  m3 @?= [shouldBe !! 3]
  m4 @?= [shouldBe !! 4]
  m5@?= [shouldBe !! 5]
  

test_convertHtml = TestCase $ do
  fixtures <- convertHtmlFile fixturesPath fromFixturesHTML
  results <- convertHtmlFile resultsPath fromResultsHTML
  let f1 = readMatches [fixtures !! 0]
  let r1 = readMatches [results !! 0]
  fixtures @?= fixturesTest
  results @?= resultsTest
  [resultsMatchesTest !! 0] @?= r1
  [fixturesMatchesTest !! 0] @?= f1


test_fillHoles = TestCase $ do
  let h = fillHoles holesTestList ""
  holesTestList2 @?= h
