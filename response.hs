module Response where

import qualified Data.Map as Map

data Response = Response {
  status :: Int,
  headers :: Map.Map String String,
  body :: String
} deriving (Show)

headersToString headers =
  let headerToString (k,v) = k ++ ": " ++ v
   in unlines $ map headerToString $ Map.toList headers

toString :: Response -> String
toString response = unlines [(show $ status response), (headersToString $ headers response), (body response)]
