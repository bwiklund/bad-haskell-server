module Server where

import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, hClose, BufferMode(..), Handle)
import Control.Concurrent (forkIO)

listen :: Int -> IO ()
listen port = withSocketsDo $ do
  sock <- listenOn $ PortNumber $ fromIntegral port
  putStrLn "listening"
  sockHandler sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
  (handle, _, _) <- accept sock
  hSetBuffering handle NoBuffering
  forkIO $ commandProcessor handle
  sockHandler sock

commandProcessor :: Handle -> IO ()
commandProcessor handle = do
  line <- hGetLine handle
  hPutStrLn handle "200 OK\n\nwelp."
  hClose handle
