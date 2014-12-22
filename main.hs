import qualified Data.Map as Map

data Request = Request { headers :: Map.Map String String, url :: String } deriving (Show)

parseRequest str =
  let headers = Map.fromList [("foo","bar")]
      url = str
      in Request headers url

main = do
  requestStr <- readFile "test/request.txt"
  let request = parseRequest requestStr
  putStrLn $ show request