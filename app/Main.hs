module Main where

import Data.Function ((&))
import Data.Text qualified as T
import Effectful (runEff)
import Effectful.Error.Static (runError)
import Servant (IsSecure (NotSecure), ServerError (ServerError))
import Servant.Auth.Server (CookieSettings (cookieIsSecure, cookieXsrfSetting), defaultCookieSettings, defaultJWTSettings, generateKey)
import System.Environment (getEnv)
import Wedding.Auth (runAuthE)
import Wedding.DB (initializeDatabase, runDBIO)
import Wedding.Server (runServerEff)

main :: IO ()
main = do
  dbFile <- getEnv "WEDDING_DATABASE"
  conn <- initializeDatabase dbFile
  password <- T.pack <$> getEnv "WEDDING_PASSWORD"
  jwtSettings <- defaultJWTSettings <$> generateKey
  let cookieSettings =
        defaultCookieSettings
          { cookieIsSecure = NotSecure,
            cookieXsrfSetting = Nothing
          }
  runServerEff cookieSettings jwtSettings
    & runAuthE password cookieSettings jwtSettings
    & runDBIO conn
    & runEff
