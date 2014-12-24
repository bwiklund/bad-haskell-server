module Server where

import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import Control.Concurrent (forkIO)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, hPutStr, hIsEOF, hClose, BufferMode(..), Handle)
import qualified Request

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

-- entry point for a request. parses headers and passes the request to the user's handler
requestHandler handle handler = do
  let request = Request.fromHandle handle
  response <- handler request
  hPutStr handle response
  hClose handle
