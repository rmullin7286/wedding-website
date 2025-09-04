module Main where

import Wedding.Env (loadEnv)
import Wedding.Server (runServer)

main :: IO ()
main = loadEnv >>= runServer
