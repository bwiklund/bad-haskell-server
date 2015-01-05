module Main (main) where

import Test.HUnit
import qualified Data.Map as Map

import Request

sampleRequest = "GET / HTTP/1.1\r\nHost: daisyowl.com\r\n\r\n"

tests = TestList [
    TestCase $ assertEqual "simplest request" (Request "GET" "/" (Map.fromList [("Host", "daisyowl.com")])) (fromString sampleRequest)
  ]

main = do
  _ <- runTestTT $ tests
  return ()
