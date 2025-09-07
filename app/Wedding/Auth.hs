{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Wedding.Auth
  ( User (..),
    LoginForm (..),
    AuthE,
    runAuthE,
    getAdminPassword,
    getCookieSettings,
    getJWTSettings,
  )
where

import Control.Lens (view)
import Data.Aeson (FromJSON, ToJSON)
import Data.Generics.Labels ()
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.Reader.Dynamic (Reader, ask, runReader)
import GHC.Generics (Generic)
import Servant.Auth.JWT (FromJWT, ToJWT)
import Servant.Auth.Server (CookieSettings, JWTSettings)
import Web.FormUrlEncoded (FromForm)

type AuthE = Reader AuthSettings

data AuthSettings = AuthSettings
  { adminPassword :: Text,
    cookieSettings :: CookieSettings,
    jwtSettings :: JWTSettings
  }
  deriving (Generic)

runAuthE :: Text -> CookieSettings -> JWTSettings -> Eff (AuthE : es) a -> Eff es a
runAuthE pass cookie jwt = runReader $ AuthSettings pass cookie jwt

getAdminPassword :: (AuthE :> es) => Eff es Text
getAdminPassword = view #adminPassword <$> ask

getCookieSettings :: (AuthE :> es) => Eff es CookieSettings
getCookieSettings = view #cookieSettings <$> ask

getJWTSettings :: (AuthE :> es) => Eff es JWTSettings
getJWTSettings = view #jwtSettings <$> ask

-- | Simple user type for admin authentication
data User = AdminUser
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToJWT, FromJWT)

-- | Login form data for password submission
data LoginForm = LoginForm
  { password :: Text
  }
  deriving (Show, Eq, Generic, FromForm)
