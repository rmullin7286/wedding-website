{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Wedding.Server (runServer) where

import Lucid (Html)
import Network.Wai.Handler.Warp (run)
import Servant (Application, Proxy (Proxy), ServerT, hoistServer, serve, serveDirectoryWebApp, (:<|>) (..))
import Servant.API (FormUrlEncoded, Get, NoContent, PlainText, Post, Raw, ReqBody, (:>))
import Servant.HTML.Lucid (HTML)
import Wedding.Config (Config (..), DBConfig (..), loadConfig)
import Wedding.Env (AppM, Env, mkEnv, runAppM)
import Wedding.Page.Home (home)
import Wedding.Page.RSVP (RSVPFormData, rsvpNameSubmission, rsvpPage)
import Wedding.DB (initializeDatabase)

-- | API Definition
type WeddingAPI =
  Get '[HTML] (Html ())
    :<|> "rsvp" :> Get '[HTML] (Html ())
    :<|> "rsvp" :> ReqBody '[FormUrlEncoded] RSVPFormData :> Post '[PlainText] NoContent
    :<|> "static" :> Raw

-- | Server implemented in AppM monad
serverAppM :: ServerT WeddingAPI AppM
serverAppM =
  return home
    :<|> return rsvpPage
    :<|> rsvpNameSubmission
    :<|> serveDirectoryWebApp "static"

weddingAPI :: Proxy WeddingAPI
weddingAPI = Proxy

app :: Env -> Application
app env = serve weddingAPI $ hoistServer weddingAPI (runAppM env) serverAppM

runServer :: FilePath -> IO ()
runServer configPath = do
  config <- loadConfig configPath
  let dbPath = location $ database config

  -- Open database connection
  conn <- initializeDatabase dbPath

  -- Create environment
  let env = mkEnv conn

  putStrLn $ "Wedding server starting on port 8080"
  putStrLn $ "Using database: " ++ dbPath
  run 8080 (app env)
