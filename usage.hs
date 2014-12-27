import Server
import Request

import System.Environment (getArgs)

main = do
  args <- getArgs
  let port = read $ head args :: Int
  listen port router

matchers = [("/echo", echoHandler)]

router :: Handler
router request = do
  (snd $ head matchers) request

echoHandler :: Handler
echoHandler request = do
  response <- request
  return ("You sent: \n\n" ++ (show response) ++ "\n\nCordially,\n  Haskell")

type Matcher = (String, Handler)

routeRequest :: [Matcher] -> Request -> IO String
routeRequest router request = do
  return ("hello this is router")
