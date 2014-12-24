module Request where

import qualified Data.Map as Map
import Data.List.Split
import Data.Maybe
import System.IO (hSetBuffering, hGetLine, hPutStrLn, hPutStr, hIsEOF, hClose, BufferMode(..), Handle)

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

readUntilEmptyLine handle acc = do
  eof <- hIsEOF handle
  if eof then return (acc)
    else do
      line <- hGetLine handle
      case line of
        "\r" -> return (reverse acc) -- TODO: this is because chrome sends \r\n. handle this more elegantly
        _ -> readUntilEmptyLine handle (line:acc)

fromHandle handle = do
  requestLines <- readUntilEmptyLine handle []
  let headerLines = tail requestLines
      headers = Map.fromList $ catMaybes $ map splitHeaderLine headerLines
      url = "foo" -- (words str) !! 1
      method = "bar" -- (words str) !! 0
   in return (Request url method headers)
