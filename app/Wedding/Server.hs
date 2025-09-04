{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Wedding.Server (runServer) where

import Lucid (Html)
import Network.Wai.Handler.Warp (run)
import Servant (Application, Proxy (Proxy), ServerT, hoistServerWithContext, serve, serveDirectoryWebApp, (:<|>) (..), err401, throwError, serveWithContext, Headers, addHeader)
import Servant.Server.Internal.Context (Context(..))
import Servant.API (FormUrlEncoded, Get, NoContent, PlainText, Post, Raw, ReqBody, (:>), Header)
import Servant.HTML.Lucid (HTML)
import Servant.Auth as SA
import Servant.Auth.Server as SAS (AuthResult(..), SetCookie, CookieSettings, JWTSettings, generateKey, defaultCookieSettings, defaultJWTSettings)
import Wedding.Auth (LoginForm(..), User(..))
import Wedding.Env (AppM, Env(..), runAppM, getAdminPassword, getJWTSettings, getCookieSettings)
import Wedding.Page.Home (home)
import Wedding.Page.RSVP (RSVPFormData, rsvpNameSubmission, rsvpPage)
import Wedding.Page.Admin (adminLogin, adminDashboard, adminLoginHandler)
import Data.Text (Text)

-- | Public API - no authentication required
type PublicAPI auths = 
  Get '[HTML] (Html ())  -- Home page
    :<|> "rsvp" :> Get '[HTML] (Html ())  -- RSVP page
    :<|> "rsvp" :> ReqBody '[FormUrlEncoded] RSVPFormData :> Post '[PlainText] NoContent  -- RSVP submission
    :<|> "admin" :> "login" :> Get '[HTML] (Html ())  -- Login form
    :<|> "admin" :> "login" :> ReqBody '[FormUrlEncoded] LoginForm :> Post '[HTML] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] (Html ()))  -- Login handler

-- | Protected API - requires authentication
type ProtectedAPI =
  "admin" :> Get '[HTML] (Html ())  -- Admin dashboard
    :<|> "admin" :> "rsvps" :> Get '[HTML] (Html ())  -- View all RSVPs

-- | Complete API with authentication
type WeddingAPI auths = 
  PublicAPI auths
    :<|> Auth auths User :> ProtectedAPI
    :<|> "static" :> Raw

-- | Public server handlers - no authentication
publicServerAppM :: ServerT (PublicAPI '[Cookie]) AppM
publicServerAppM = 
  return home
    :<|> return rsvpPage
    :<|> rsvpNameSubmission
    :<|> return adminLogin
    :<|> adminLoginHandler

-- | Protected server handlers - requires authentication
protectedServerAppM :: AuthResult User -> ServerT ProtectedAPI AppM
protectedServerAppM (Authenticated _user) =
  return adminDashboard
    :<|> return adminDashboard  -- TODO: Replace with RSVP list page
protectedServerAppM _ = throwError err401 :<|> throwError err401

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
