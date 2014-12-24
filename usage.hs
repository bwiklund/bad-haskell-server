import Server

import System.Environment (getArgs)

main = do
  args <- getArgs
  let port = read $ head args :: Int
  listen port fooHandler

fooHandler request = "200 OK\n\nwelp."
