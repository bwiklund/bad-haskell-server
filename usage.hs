import Server
import Router
import Response

import System.Environment (getArgs)
import qualified Data.Map

fooRouter = Router [("/echo", echoHandler)]

echoHandler :: Handler
echoHandler request = do
  let response = request
      body = "You sent: \n\n" ++ (show response) ++ "\n\nCordially,\n  Haskell"
  return (Response 200 Data.Map.empty body)

main = do
  args <- getArgs
  let port = read $ head args :: Int
  listen port $ routeRequest fooRouter
