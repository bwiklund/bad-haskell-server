import Server
import Router

import System.Environment (getArgs)

fooRouter = Router [("/echo", echoHandler)]

main = do
  args <- getArgs
  let port = read $ head args :: Int
  listen port $ routeRequest fooRouter

echoHandler :: Handler
echoHandler request = do
  let response = request
  return ("HTTP/1.1 200 OK\nContent-Type: text/plain\n\nYou sent: \n\n" ++ (show response) ++ "\n\nCordially,\n  Haskell")
