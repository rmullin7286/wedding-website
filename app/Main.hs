module Main where

import Data.Text qualified as T
import Servant (IsSecure (NotSecure))
import Servant.Auth.Server (CookieSettings (cookieIsSecure, cookieXsrfSetting), defaultCookieSettings, defaultJWTSettings, generateKey)
import System.Environment (getEnv)
import Wedding.DB (initializeDatabase)
import Wedding.Env (Env(..))
import Wedding.Server (runServer)

-- | Method to load environment from the system.
-- Should be called at the beginning of the program before the server starts.
loadEnv :: IO Env
loadEnv =
  Env
    <$> (getEnv "WEDDING_DATABASE" >>= initializeDatabase)
    <*> (T.pack <$> getEnv "WEDDING_PASSWORD")
    <*> (defaultJWTSettings <$> generateKey)
    <*> pure developmentCookieSettings
  where
    -- Cookie settings that work with HTTP (development) - XSRF disabled for simplicity
    developmentCookieSettings =
      defaultCookieSettings
        { cookieIsSecure = NotSecure, -- Allow cookies over HTTP
          cookieXsrfSetting = Nothing -- Disable XSRF - Need to figure out exactly how xsrf works
        }

main :: IO ()
main = loadEnv >>= runServer
