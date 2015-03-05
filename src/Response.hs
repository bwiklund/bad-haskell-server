module Response where

import qualified Data.Map as Map
import Data.List (intercalate)
import Data.Maybe

data Response = Response {
  status :: Int,
  headers :: Map.Map String String,
  body :: String
} deriving (Show, Eq)

unlinesCrlf = intercalate "\r\n"

statusCodes =
  Map.fromList [
    (200, "OK"),
    (404, "Not found"),
    (500, "Server error")
  ]

lookupStatusMessage code =
  fromMaybe "Unknown status code" (Map.lookup code statusCodes)

headersToString headers =
  let headerToString (k,v) = k ++ ": " ++ v
   in unlinesCrlf $ map headerToString $ Map.toList headers

-- i could just make an instance of Show, but that would make it hard to debug by logging.
-- or maybe not. i'm on a plane and don't have wifi.
toString :: Response -> String
toString response =
  unlinesCrlf [
    unwords [
      "HTTP/1.1",
      show $ status response,
      lookupStatusMessage $ status response
    ],
    headersToString $ headers response,
    "",
    body response
  ]
