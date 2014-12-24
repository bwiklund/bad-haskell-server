import qualified Data.Map as Map
import Data.List.Split
import Data.Maybe

data Request = Request {
  url :: String,
  method :: String,
  headers :: Map.Map String String
} deriving (Show)

splitHeaderLine :: String -> Maybe (String, String)
splitHeaderLine "" = Nothing
splitHeaderLine line =
  let parts = splitOn ": " line
   in Just (parts !! 0, parts !! 1)

fromStr str =
  let headerLines = tail $ lines str
      headers = Map.fromList $ catMaybes $ map splitHeaderLine headerLines
      url = (words str) !! 1
      method = (words str) !! 0
   in Request url method headers
