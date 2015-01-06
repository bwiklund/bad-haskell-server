module Request where

import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.Maybe
import System.IO (hGetLine, hIsEOF)

data Request = Request {
  method :: String,
  uri :: String,
  headers :: Map.Map String String
} deriving (Show, Eq)

-- used both in the IO code and pure code, for simplicty with running / test suite
filterCrs = filter (/='\r')

splitHeaderLine :: String -> Maybe (String, String)
splitHeaderLine "" = Nothing
splitHeaderLine line =
  let (x:xs) = splitOn ": " line
   in Just (x, head xs)

-- we can't simply read the request to the end, because browsers keep the line open
readUntilEmptyLine handle acc = do
  eof <- hIsEOF handle
  if eof then return (unlines $ reverse acc)
    else do
      rawLine <- hGetLine handle
      let line = filterCrs rawLine
      case line of
        "" -> return (unlines $ reverse acc)
        _ -> readUntilEmptyLine handle (line:acc)

-- TODO: instance Read here?
fromString :: String -> Request
fromString str =
  let filteredStr = filterCrs str
      (requestLine:headerLines) = lines filteredStr
      (method:uri:_) = words requestLine
      headers = Map.fromList $ catMaybes $ map splitHeaderLine headerLines
   in Request method uri headers

fromHandle handle = do
  requestStr <- readUntilEmptyLine handle []
  return $ fromString requestStr
