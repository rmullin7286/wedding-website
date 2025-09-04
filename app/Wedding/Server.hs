{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Wedding.Server (runServer) where

import Lucid (Html)
import Network.Wai.Handler.Warp (run)
import Servant (Application, Proxy (Proxy), ServerT, hoistServer, serve, serveDirectoryWebApp, (:<|>) (..))
import Servant.API (FormUrlEncoded, Get, NoContent, PlainText, Post, Raw, ReqBody, (:>))
import Servant.HTML.Lucid (HTML)
import Wedding.Env (AppM, Env, runAppM)
import Wedding.Page.Home (home)
import Wedding.Page.RSVP (RSVPFormData, rsvpNameSubmission, rsvpPage)

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

runServer :: Env -> IO ()
runServer env = run 8080 (app env)
