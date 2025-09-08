{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Wedding.Auth
  ( User (..),
    LoginForm (..),
    AuthE,
    runAuthE,
    getAdminPassword,
    getCookieSettings,
    getJWTSettings,
    acceptLogin,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Generics.Labels ()
import Data.Text (Text)
import Effectful (Eff, Effect, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (interpretWith)
import Effectful.TH (makeEffect)
import GHC.Generics (Generic)
import Servant (AddHeader)
import Servant.Auth.JWT (FromJWT, ToJWT)
import Servant.Auth.Server (CookieSettings, JWTSettings, SetCookie)
import Servant.Auth.Server qualified as SAS
import Web.FormUrlEncoded (FromForm)

data AuthE :: Effect where
  GetAdminPassword :: AuthE m Text
  GetCookieSettings :: AuthE m CookieSettings
  GetJWTSettings :: AuthE m JWTSettings
  AcceptLogin ::
    ( AddHeader mods "Set-Cookie" SetCookie response withOneCookie,
      AddHeader mods "Set-Cookie" SetCookie withOneCookie withTwoCookies
    ) =>
    AuthE m (Maybe (response -> withTwoCookies))

makeEffect ''AuthE

data AuthSettings = AuthSettings
  { adminPassword :: Text,
    cookieSettings :: CookieSettings,
    jwtSettings :: JWTSettings
  }
  deriving (Generic)

runAuthE :: (IOE :> es) => Text -> CookieSettings -> JWTSettings -> Eff (AuthE : es) a -> Eff es a
runAuthE pass cookie jwt action = interpretWith action $ \_ -> \case
  GetAdminPassword -> return pass
  GetCookieSettings -> return cookie
  GetJWTSettings -> return jwt
  AcceptLogin -> liftIO $ SAS.acceptLogin cookie jwt AdminUser

-- | Simple user type for admin authentication
data User = AdminUser
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToJWT, FromJWT)

-- | Login form data for password submission
data LoginForm = LoginForm
  { password :: Text
  }
  deriving (Show, Eq, Generic, FromForm)
