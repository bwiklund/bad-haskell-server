import Server

import System.Environment (getArgs)

main = do
  args <- getArgs
  let port = read $ head args :: Int
  listen port echoHandler

echoHandler request = "You sent: \n\n" ++ request ++ "\n\nCordially,\n  Haskell"
