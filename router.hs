module Router where

import Server
import Request

import Text.Regex.Posix
import Data.List (find)

type Matcher = (String, Handler)
data Router = Router {matchers :: [Matcher]}

routeRequest router = \request -> do
  let matchedHandler = find (\(re,_) -> (uri request) =~ re :: Bool) (matchers router)
  case matchedHandler of
    Just handler -> (snd handler) request
    Nothing -> return ("HTTP/1.1 404 Non Found\nContent-Type: text/plain\n\n404 Can't Even")
