module Response where

import qualified Data.Map as Map

data Response = Response {
  status :: Int,
  headers :: Map.Map String String,
  body :: String
} deriving (Show)

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

headersToString headers =
  let headerToString (k,v) = k ++ ": " ++ v
   in unlines $ map headerToString $ Map.toList headers

-- i could just make an instance of Show, but that would make it hard to debug by logging.
-- or maybe not. i'm on a plane and don't have wifi.
toString :: Response -> String
toString response =
  unlines [
    unwords ["HTTP/1.1", show $ status response, lookupStatusMessage $ status response],
    headersToString $ headers response,
    body response
  ]
