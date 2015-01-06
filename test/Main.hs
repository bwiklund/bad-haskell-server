module Main (main) where

import Test.HUnit
import qualified Data.Map as Map

import Request
import Response
import Router

testMatcher = ("^/$", (\request -> return $ Response 200 Map.empty "bar"))
testRouter = Router [testMatcher]

tests = TestList [

    TestCase $ assertEqual
      "simplest request"
      (Request "GET" "/" (Map.fromList [("Host", "daisyowl.com")]))
      (fromString "GET / HTTP/1.1\r\nHost: daisyowl.com\r\n\r\n"),

    TestCase $ assertEqual
      "simplest response"
      "HTTP/1.1 200 OK\r\nContent-type: text/plain\r\n\r\nOh hai there"
      (toString $ Response 200 (Map.fromList [("Content-type", "text/plain")]) "Oh hai there"),

    TestCase (do
      response <- routeRequest testRouter (Request "GET" "/" Map.empty)
      assertEqual "simplest routing" "bar" (body response)),

    TestCase (do
      response <- routeRequest testRouter (Request "GET" "/foo" Map.empty)
      assertEqual "404s on routing failure" 404 (status response))

  ]

main = do
  _ <- runTestTT $ tests
  return ()
