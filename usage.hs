import Server
import Router
import Response

import System.Environment (getArgs)
import qualified Data.Map as Map
import qualified System.Time as Time

fooRouter = Router [
    ("/echo", echoHandler),
    ("/time", timeHandler)
  ]

echoHandler request =
  let body = "You sent: \n\n" ++ (show request) ++ "\n\nCordially,\n  Haskell"
   in return (Response 200 Map.empty body)

timeHandler request = do
  now <- Time.getClockTime
  return (Response 200 Map.empty $ show now)

main = do
  args <- getArgs
  let port = read $ head args :: Int
  listen port $ routeRequest fooRouter
