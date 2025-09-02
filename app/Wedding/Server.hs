{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Wedding.Server (runServer) where

import Lucid (Html)
import Network.Wai.Handler.Warp (run)
import Servant (Application, Proxy (Proxy), Server, serve, serveDirectoryWebApp, (:<|>) (..))
import Servant.API (FormUrlEncoded, Get, NoContent, PlainText, Post, Raw, ReqBody, (:>))
import Servant.HTML.Lucid (HTML)
import Wedding.Page.Home (home)
import Wedding.Page.RSVP (RSVPFormData, rsvpNameSubmission, rsvpPage)

-- | API Definition
type WeddingAPI =
  Get '[HTML] (Html ())
    :<|> "rsvp" :> Get '[HTML] (Html ())
    :<|> "rsvp" :> ReqBody '[FormUrlEncoded] RSVPFormData :> Post '[PlainText] NoContent
    :<|> "static" :> Raw

server :: Server WeddingAPI
server =
  return home
    :<|> return rsvpPage
    :<|> rsvpNameSubmission
    :<|> serveDirectoryWebApp "static"

weddingAPI :: Proxy WeddingAPI
weddingAPI = Proxy

app :: Application
app = serve weddingAPI server

runServer :: FilePath -> IO ()
runServer configFile = do
  -- For now, just acknowledge the config file parameter
  -- Actual YAML loading will be implemented later
  putStrLn $ "Server starting with config: " ++ configFile
  run 8080 app
