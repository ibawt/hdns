module Main where

import Server

main :: IO ()
main = do
  putStrLn "Listening..."
  listenAndServe
