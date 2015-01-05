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

splitHeaderLine :: String -> Maybe (String, String)
splitHeaderLine "" = Nothing
splitHeaderLine "\r" = Nothing
splitHeaderLine line =
  let (x:xs) = splitOn ": " line
   in Just (x, head xs)

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

-- TODO: instance Read here?
fromString :: String -> Request
fromString str =
  let (requestLine:headerLines) = lines str
      (method:uri:_) = words requestLine
      headers = Map.fromList $ catMaybes $ map splitHeaderLine headerLines
   in Request method uri headers

fromHandle handle = do
  str <- readUntilEmptyLine handle []
  return $ fromString str
