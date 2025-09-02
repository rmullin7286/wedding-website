module Main where

import Wedding.CLI (Options(..), parseOptions)
import Wedding.Server (runServer)

main :: IO ()
main = do
  options <- parseOptions
  putStrLn $ "Using config file: " ++ configFile options
  runServer (configFile options)
