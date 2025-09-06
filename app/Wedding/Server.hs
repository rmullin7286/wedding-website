{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Wedding.Server (runServer) where

import Lucid (Html)
import Network.Wai.Handler.Warp (run)
import Servant (Application, Headers, Proxy (Proxy), ServerT, err401, hoistServerWithContext, serveDirectoryWebApp, serveWithContext, throwError, (:<|>) (..))
import Servant.API (FormUrlEncoded, Get, Header, NoContent, PlainText, Post, Raw, ReqBody, (:>))
import Servant.Auth as SA
import Servant.Auth.Server as SAS (AuthResult (..), CookieSettings, JWTSettings, SetCookie)
import Servant.HTML.Lucid (HTML)
import Servant.Multipart (MultipartForm, Tmp)
import Servant.Server.Internal.Context (Context (..))
import Wedding.Auth (LoginForm (..), User (..))
import Wedding.Env (AppM, Env (..), runAppM)
import Wedding.Page.Admin (CsvUpload, adminDashboard, adminLogin, adminLoginHandler, csvUploadHandler)
import Wedding.Page.Home (home)
import Wedding.Page.RSVP (RSVPFormData, GroupRSVPFormData, rsvpNameSubmission, rsvpGroupSubmission, rsvpPage)

-- | Public API - no authentication required
type PublicAPI auths =
  Get '[HTML] (Html ()) -- Home page
    :<|> "rsvp" :> Get '[HTML] (Html ()) -- RSVP page
    :<|> "rsvp" :> ReqBody '[FormUrlEncoded] Wedding.Page.RSVP.RSVPFormData :> Post '[HTML] (Html ()) -- RSVP name submission -> group form
    :<|> "rsvp" :> "submit" :> ReqBody '[FormUrlEncoded] Wedding.Page.RSVP.GroupRSVPFormData :> Post '[HTML] (Html ()) -- Final RSVP submission
    :<|> "admin" :> "login" :> Get '[HTML] (Html ()) -- Login form
    :<|> "admin" :> "login" :> ReqBody '[FormUrlEncoded] LoginForm :> Post '[HTML] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] (Html ())) -- Login handler

-- | Protected API - requires authentication
type ProtectedAPI =
  "admin" :> Get '[HTML] (Html ()) -- Admin dashboard
    :<|> "admin" :> "upload-csv" :> MultipartForm Tmp CsvUpload :> Post '[HTML] (Html ()) -- CSV upload

-- | Complete API with authentication
type WeddingAPI auths =
  PublicAPI auths
    :<|> Auth auths User :> ProtectedAPI
    :<|> "static" :> Raw

-- | Public server handlers - no authentication
publicServerAppM :: ServerT (PublicAPI '[Cookie]) AppM
publicServerAppM =
  return home
    :<|> return Wedding.Page.RSVP.rsvpPage
    :<|> Wedding.Page.RSVP.rsvpNameSubmission
    :<|> Wedding.Page.RSVP.rsvpGroupSubmission
    :<|> return adminLogin
    :<|> adminLoginHandler

-- | Protected server handlers - requires authentication
protectedServerAppM :: AuthResult User -> ServerT ProtectedAPI AppM
protectedServerAppM (Authenticated _user) =
  adminDashboard
    :<|> csvUploadHandler
protectedServerAppM _ = throwError err401 :<|> (\_ -> throwError err401)

-- | Complete server with authentication
serverAppM :: ServerT (WeddingAPI '[Cookie]) AppM
serverAppM =
  publicServerAppM
    :<|> protectedServerAppM
    :<|> serveDirectoryWebApp "static"

weddingAPI :: Proxy (WeddingAPI '[Cookie])
weddingAPI = Proxy

app :: CookieSettings -> JWTSettings -> Env -> Application
app cookieSettings jwtSettings env =
  let cfg = cookieSettings :. jwtSettings :. EmptyContext
   in serveWithContext weddingAPI cfg $ hoistServerWithContext weddingAPI (Proxy :: Proxy '[CookieSettings, JWTSettings]) (runAppM env) serverAppM

runServer :: Env -> IO ()
runServer env@(Env _ _ jwtSettings cookieSettings) =
  run 8080 (app cookieSettings jwtSettings env)
