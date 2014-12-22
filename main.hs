import qualified Data.Map as Map

data Request = Request {
  url :: String,
  method :: String,
  headers :: Map.Map String String
} deriving (Show)

parseRequest str =
  let headers = Map.fromList [("foo","bar")]
      url = str
      method = "GET"
      in Request url method headers

main = do
  requestStr <- readFile "test/request.txt"
  let request = parseRequest requestStr
  putStrLn $ show request