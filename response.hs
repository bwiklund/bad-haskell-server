module Response where

import qualified Data.Map as Map

statusCodes =
  Map.fromList [
    (200, "OK"),
    (400, "Not found"),
    (500, "Server error")
  ]

lookupStatusMessage code =
  case (Map.lookup code statusCodes) of
    Nothing -> "Shrug"
    Just message -> message

data Response = Response {
  status :: Int,
  headers :: Map.Map String String,
  body :: String
} deriving (Show)

headersToString headers =
  let headerToString (k,v) = k ++ ": " ++ v
   in unlines $ map headerToString $ Map.toList headers

toString :: Response -> String
toString response =
  unlines [
    unwords ["HTTP/1.1", show $ status response, lookupStatusMessage $ status response],
    headersToString $ headers response,
    body response
  ]
