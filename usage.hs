import Server
import Router
import Response

import System.Environment (getArgs)
import qualified Data.Map as Map

fooRouter = Router [("/echo", echoHandler)]

echoHandler request = do
  let body = "You sent: \n\n" ++ (show request) ++ "\n\nCordially,\n  Haskell"
  return (Response 200 Map.empty body)

main = do
  args <- getArgs
  let port = read $ head args :: Int
  listen port $ routeRequest fooRouter
