module Server where

import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, hPutStr, hIsEOF, hClose, BufferMode(..), Handle)
import Control.Concurrent (forkIO)

-- starts a server
listen port handler = withSocketsDo $ do
  sock <- listenOn $ PortNumber $ fromIntegral port
  putStrLn "listening"
  sockHandler sock handler

-- recursively accepts connections and forks a handler
sockHandler sock handler = do
  (handle, _, _) <- accept sock
  hSetBuffering handle NoBuffering
  forkIO $ requestHandler handle handler
  sockHandler sock handler

readUntilEmptyLine handle acc = do
  eof <- hIsEOF handle
  if eof then return (acc)
    else do
      line <- hGetLine handle
      case line of
        "\r" -> return (reverse acc) -- TODO: this is because chrome sends \r\n. handle this more elegantly
        _ -> readUntilEmptyLine handle (line:acc)

-- entry point for a request. parses headers and passes the request to the user's handler
requestHandler handle handler = do
  headerLines <- readUntilEmptyLine handle []
  let request = unlines headerLines
  let response = handler request
  hPutStr handle response
  hClose handle
