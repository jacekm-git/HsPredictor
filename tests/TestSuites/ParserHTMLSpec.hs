module TestSuites.ParserHTMLSpec(spec, matches) where
--3rd party
import Test.Hspec.Contrib.HUnit(fromHUnitTest)
import Test.Hspec (hspec)
import Test.HUnit
--own
import ParserHTML

main = hspec spec

spec = fromHUnitTest $ TestList [
  TestLabel ">>readMatches" test_readMatches]


matches = [
  "15.08.2014, Swansea - Newcastle Utd, 1.92, 3.52, 4.06",
  "30.08.2014, Everton - Chelsea, 3:6, 4.05, 3.42, 1.96",
  "31.08.2014, Everton - Chelsea, 3:6, 4.05",
  "32.08.2014, Everton - Chelsea, 3:6",
  "33.08.2014, Everton - Chelsea",
  "34.08.2014, Everton - Chelsea,"]
  
shouldBe = [
  "2014.08.15,Swansea,Newcastle Utd,-1,-1,1.92,3.52,4.06 \n",
  "2014.08.30,Everton,Chelsea,3,6,4.05,3.42,1.96 \n",
  "2014.08.31,Everton,Chelsea,3,6,-1.0,-1.0,-1.0 \n",
  "2014.08.32,Everton,Chelsea,3,6,-1.0,-1.0,-1.0 \n",
  "2014.08.33,Everton,Chelsea,-1,-1,-1.0,-1.0,-1.0 \n",
  "2014.08.34,Everton,Chelsea,-1,-1,-1.0,-1.0,-1.0 \n"]
  
  
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
  

