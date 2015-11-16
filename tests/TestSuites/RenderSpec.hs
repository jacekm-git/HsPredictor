module TestSuites.RenderSpec (spec) where

import Test.Hspec.Contrib.HUnit(fromHUnitTest)
import Test.HUnit
import Test.Hspec               (hspec)

import HsPredictor.Render

main = hspec spec

spec = fromHUnitTest $ TestList [
  TestLabel "" test_renderList,
  TestLabel "" test_convertList
  ]

stats = [("A", [3, 2 ,1]), ("B", [2, 3, 1])]

l = [["A", "1", "2", "3"]]

test_renderList = TestCase $ do
  let t = renderList l 3
  t @?= ["|  A|  1|  2|  3|", "-----------------"]

test_convertList = TestCase $ do
  let t = convertList stats
  t @?= [["Team", "Win", "Draw", "Loss"],
         ["A", "3", "2", "1"],
         ["B", "2", "3", "1"]]
