module TestSuites.RenderSpec (spec) where

import Test.Hspec.Contrib.HUnit(fromHUnitTest)
import Test.HUnit
import Test.Hspec               (hspec)

import HsPredictor.Render

main = hspec spec

spec = fromHUnitTest $ TestList [
  TestLabel "" test_renderTable
  ]

stats = [("A", [3, 2 ,1])
        ,("B", [2, 3, 1])
        ,("C", [1, 4, 1])]

test_renderTable = TestCase $ do
  let t = listToTable stats
  t @?= "A 3 2 1\nB 2 3 1\nC 1 4 1\n"
