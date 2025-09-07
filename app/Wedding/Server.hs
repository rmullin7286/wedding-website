{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Wedding.Server (runServerEff) where

import Data.Function ((&))
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.FileSystem (FileSystem)
import Effectful.Servant (runWarpServerSettingsContext)
import Lucid (Html)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Servant (Headers, ServerError, ServerT, err401, serveDirectoryWebApp, (:<|>) (..))
import Servant.API (FormUrlEncoded, Get, Header, Post, Raw, ReqBody)
import Servant.API qualified as S
import Servant.Auth as SA
import Servant.Auth.Server as SAS (AuthResult (..), CookieSettings, JWTSettings, SetCookie)
import Servant.HTML.Lucid (HTML)
import Servant.Multipart (MultipartForm, Tmp)
import Servant.Server.Internal.Context (Context (..))
import Wedding.Auth (AuthE, LoginForm (..), User (..))
import Wedding.DB (DB)
import Wedding.Page.Admin (CsvUpload, adminDashboard, adminLogin, adminLoginHandler, csvUploadHandler)
import Wedding.Page.Home (home)
import Wedding.Page.RSVP (GroupRSVPFormData, RSVPFormData, rsvpGroupSubmission, rsvpNameSubmission, rsvpPage)

-- | Public API - no authentication required
type PublicAPI auths =
  Get '[HTML] (Html ()) -- Home page
    :<|> "rsvp" S.:> Get '[HTML] (Html ()) -- RSVP page
    :<|> "rsvp" S.:> ReqBody '[FormUrlEncoded] Wedding.Page.RSVP.RSVPFormData S.:> Post '[HTML] (Html ()) -- RSVP name submission -> group form
    :<|> "rsvp" S.:> "submit" S.:> ReqBody '[FormUrlEncoded] Wedding.Page.RSVP.GroupRSVPFormData S.:> Post '[HTML] (Html ()) -- Final RSVP submission
    :<|> "admin" S.:> "login" S.:> Get '[HTML] (Html ()) -- Login form
    :<|> "admin" S.:> "login" S.:> ReqBody '[FormUrlEncoded] LoginForm S.:> Post '[HTML] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] (Html ())) -- Login handler

-- | Protected API - requires authentication
type ProtectedAPI =
  "admin" S.:> Get '[HTML] (Html ()) -- Admin dashboard
    :<|> "admin" S.:> "upload-csv" S.:> MultipartForm Tmp CsvUpload S.:> Post '[HTML] (Html ()) -- CSV upload

-- | Complete API with authentication
type WeddingAPI auths =
  PublicAPI auths
    :<|> Auth auths User S.:> ProtectedAPI
    :<|> "static" S.:> Raw

-- | Public server handlers - no authentication
publicServerAppM :: (AuthE :> es, DB :> es, Error ServerError :> es, IOE :> es) => ServerT (PublicAPI '[Cookie]) (Eff es)
publicServerAppM =
  return home
    :<|> return Wedding.Page.RSVP.rsvpPage
    :<|> Wedding.Page.RSVP.rsvpNameSubmission
    :<|> Wedding.Page.RSVP.rsvpGroupSubmission
    :<|> return adminLogin
    :<|> adminLoginHandler

-- | Protected server handlers - requires authentication
protectedServerAppM :: (FileSystem :> es, DB :> es, Error ServerError :> es, IOE :> es, AuthE :> es) => AuthResult User -> ServerT ProtectedAPI (Eff es)
protectedServerAppM (Authenticated _user) =
  adminDashboard
    :<|> csvUploadHandler
protectedServerAppM _ = throwError err401 :<|> (\_ -> throwError err401)

-- | Complete server with authentication
serverAppM :: (DB :> es, Error ServerError :> es, IOE :> es, AuthE :> es, FileSystem :> es) => ServerT (WeddingAPI '[Cookie]) (Eff es)
serverAppM =
  publicServerAppM
    :<|> protectedServerAppM
    :<|> serveDirectoryWebApp "static"

runServerEff :: (AuthE :> es, DB :> es, IOE :> es, FileSystem :> es) => CookieSettings -> JWTSettings -> Eff es ()
runServerEff cookieSettings jwtSettings = runWarpServerSettingsContext @(WeddingAPI '[Cookie]) settings ctx serverAppM id
  where
    settings = defaultSettings & setPort 8080
    ctx = cookieSettings :. jwtSettings :. EmptyContext
