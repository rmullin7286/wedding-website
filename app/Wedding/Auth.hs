{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Wedding.Auth (User(..), LoginForm(..)) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Web.FormUrlEncoded (FromForm)
import Data.Text (Text)
import Servant.Auth.JWT (ToJWT, FromJWT)

-- | Simple user type for admin authentication
data User = AdminUser
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToJWT, FromJWT)

-- | Login form data for password submission
data LoginForm = LoginForm
  { password :: Text
  } deriving (Show, Eq, Generic, FromForm)