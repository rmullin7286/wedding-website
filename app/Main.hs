{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Wedding.CLI (Options (..), parseOptions)
import Wedding.Config (Config (..), DBConfig (..), loadConfig)
import Wedding.DB (initializeDatabase)
import Wedding.Env (mkEnv)
import Wedding.Server (runServer)

main :: IO ()
main = do
  options <- parseOptions
  config <- loadConfig options.configFile
  conn <- initializeDatabase config.database.location
  let env = mkEnv conn
  runServer env
