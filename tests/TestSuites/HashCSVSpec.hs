module TestSuites.HashCSVSpec (spec) where

import Test.Hspec.Contrib.HUnit(fromHUnitTest)
import Test.HUnit
import HashCSV

spec = fromHUnitTest $ TestList [
  TestLabel ">>genHash" test_genHash
  , TestLabel ">>checkHash" test_checkHash
  ]

test_genHash = TestCase $ do
  h <- genHash "test_string\n"
  let h' = "fd77c0776e992fc96647b3bc220b3adc"
  assertEqual "Testing hash generator" h h'

test_checkHash = TestCase $ do
  let h = "abc"
  let h' = ""
  assertBool "True if h/=h'"$ checkHash h h'
  assertBool "False if h==h"$ not $ checkHash h h
