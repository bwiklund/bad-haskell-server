module Main (main) where

import Test.HUnit

tests = TestList [
    TestCase $ assertEqual "2 plus 2" 4 (2+2)
  ]

main = do
  _ <- runTestTT $ tests
  return ()
