import Server
import Router
import Response

import System.Environment (getArgs)
import qualified Data.Map

fooRouter = Router [("/echo", echoHandler)]

main = do
  args <- getArgs
  let port = read $ head args :: Int
  listen port $ routeRequest fooRouter

echoHandler :: Handler
echoHandler request = do
  let response = request
  return (Response 200 Data.Map.empty ("You sent: \n\n" ++ (show response) ++ "\n\nCordially,\n  Haskell"))
