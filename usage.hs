import Server
import Request

import System.Environment (getArgs)
import Text.Regex.Posix
import Data.List (find)

main = do
  args <- getArgs
  let port = read $ head args :: Int
  listen port router

type Matcher = (String, Handler)

matchers = [("/echo", echoHandler)]

router :: Handler
router request = do
  let matchedHandler = find (\(re,_) -> (uri request) =~ re :: Bool) matchers
  case matchedHandler of
    Just handler -> (snd handler) request
    Nothing -> return ("HTTP/1.1 404 Non Found\nContent-Type: text/plain\n\n404 Can't Even")

echoHandler :: Handler
echoHandler request = do
  let response = request
  return ("HTTP/1.1 200 OK\nContent-Type: text/plain\n\nYou sent: \n\n" ++ (show response) ++ "\n\nCordially,\n  Haskell")
