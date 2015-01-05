module Request where

import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.Maybe
import System.IO (hGetLine, hIsEOF)

data Request = Request {
  uri :: String,
  method :: String,
  headers :: Map.Map String String
} deriving (Show)

splitHeaderLine :: String -> Maybe (String, String)
splitHeaderLine "" = Nothing
splitHeaderLine line =
  let (k:v:_) = splitOn ": " line
   in Just (k, v)

-- TODO: treat as chars, not lines?
readUntilEmptyLine handle acc = do
  eof <- hIsEOF handle
  if eof then return (unlines $ reverse acc)
    else do
      line <- hGetLine handle
      -- putStrLn $ show $ map show (line :: [Char]) -- for debugging /r and /n
      case line of
        "\r" -> return (unlines $ reverse acc) -- TODO: this is because chrome sends \r\n. handle this more elegantly
        _ -> readUntilEmptyLine handle (line:acc)

fromString :: String -> Request
fromString str =
  let (requestLine:headerLines) = lines str
      (method:uri:_) = words requestLine
      headers = Map.fromList $ catMaybes $ map splitHeaderLine headerLines
   in Request uri method headers

fromHandle handle = do
  str <- readUntilEmptyLine handle []
  return (fromString str)
