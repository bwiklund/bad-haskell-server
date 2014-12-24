import Server

import System.Environment (getArgs)

main = do
  args <- getArgs
  let port = read $ head args :: Int
  listen port echoHandler

echoHandler request = do
  response <- request
  return ("You sent: \n\n" ++ (show response) ++ "\n\nCordially,\n  Haskell")
