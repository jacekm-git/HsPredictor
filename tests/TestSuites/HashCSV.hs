module TestSuites.HashCSV (tests_HashCSV) where

import Test.HUnit
import HashCSV

tests_HashCSV = TestList [
  test_genHash
  , test_checkHash
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
