module Server where

import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import Control.Concurrent (forkIO)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, hPutStr, hIsEOF, hClose, BufferMode(..), Handle)
import Request

type Handler = (IO Request -> IO String)

-- starts a server
listen :: Int -> Handler -> IO ()
listen port handler = withSocketsDo $ do
  sock <- listenOn $ PortNumber $ fromIntegral port
  putStrLn "listening"
  sockHandler sock handler

-- recursively accepts connections and forks a handler
sockHandler :: Socket -> Handler -> IO ()
sockHandler sock handler = do
  (handle, _, _) <- accept sock
  hSetBuffering handle NoBuffering
  forkIO $ requestHandler handle handler
  sockHandler sock handler

-- entry point for a request. parses headers and passes the request to the user's handler
requestHandler :: Handle -> Handler -> IO ()
requestHandler handle handler = do
  let request = fromHandle handle
  response <- handler request
  hPutStr handle response
  hClose handle
