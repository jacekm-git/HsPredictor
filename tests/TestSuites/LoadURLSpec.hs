module TestSuites.LoadURLSpec(spec) where
--standard
import Data.List (isInfixOf)
--3rd party
import Test.Hspec.Contrib.HUnit(fromHUnitTest)
import Test.Hspec (hspec)
import Test.HUnit
--own
import LoadURL

main = hspec spec
spec = fromHUnitTest $ TestList [
  TestLabel ">>getBody" test_getBody
  ]

testUrl = "http://www.blank.org"

test_getBody = TestCase $ do
  b <- getBody testUrl
  "<title>\nblank\n</title>" `isInfixOf` b @?= True
  b1 <- getBody "wrong url"
  b1 @?= ""
