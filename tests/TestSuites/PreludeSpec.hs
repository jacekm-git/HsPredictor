module TestSuites.PreludeSpec(spec) where
--3rd party
import           Test.Hspec               (hspec)
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit
--own
import           HsPredictor.Prelude

main = hspec spec
spec = fromHUnitTest $ TestList [
  TestLabel ">>sHead" test_sHead,
  TestLabel ">>!?" test_Index
  ]

emptyList :: [Int]
emptyList = []

test_sHead = TestCase $ do
  Nothing @?= (sHead emptyList)
  (Just 1) @?= (sHead [1])
  (Just 1) @?= (sHead [1,2])

test_Index = TestCase $ do
  Nothing @?= (emptyList !? 0)
  Nothing @?= ([0] !? 1)
  (Just 1) @?= ([1,2] !? 0)
  (Just 2) @?= ([1,2] !? 1)
