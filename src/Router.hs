module Router where

import Server
import Request
import Response

import Text.Regex.Posix
import Data.List (find)
import qualified Data.Map as Map

type Matcher = (String, Handler)
data Router = Router {matchers :: [Matcher]}

routeRequest router request =
  let matchedHandler = find (\(re,_) -> (uri request) =~ re :: Bool) (matchers router)
   in case matchedHandler of
      Just handler -> (snd handler) request
      Nothing -> return (Response 404 Map.empty "I can't even")
