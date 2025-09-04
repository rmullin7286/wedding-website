{-# LANGUAGE DataKinds #-}

module Wedding.Page.Admin (adminLogin, adminDashboard, adminLoginHandler) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Lucid
import Servant (Headers, addHeader, err302, err401, throwError)
import Servant.API (Header)
import Servant.Auth.Server (SetCookie, acceptLogin)
import Wedding.Auth (LoginForm (..), User (..))
import Wedding.Component.BasePage (basePage)
import Wedding.Env (AppM, getAdminPassword, getCookieSettings, getJWTSettings)

-- | Admin login form page
adminLogin :: Html ()
adminLogin = basePage "Admin Login" $ do
  div_ [class_ "container mx-auto px-4 py-16"] $ do
    div_ [class_ "max-w-md mx-auto bg-white rounded-lg shadow-md p-6"] $ do
      h1_ [class_ "text-2xl font-bold mb-6 text-center", style_ "font-family: 'Playfair Display', serif;"] "Admin Login"
      form_ [method_ "post", action_ "/admin/login"] $ do
        div_ [class_ "mb-6"] $ do
          label_ [for_ "password", class_ "block text-sm font-medium text-gray-700 mb-3"] "Password"
          input_
            [ type_ "password",
              id_ "password",
              name_ "password",
              class_ "w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500",
              required_ "required"
            ]
        div_ [class_ "flex justify-center"] $
          button_
            [ type_ "submit",
              class_ "text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline",
              style_ "background-color: rgba(122,39,61,255); hover:background-color: rgba(102,29,51,255);"
            ]
            "Login"

-- | Admin dashboard page
adminDashboard :: Html ()
adminDashboard = basePage "Admin Dashboard" $ do
  div_ [class_ "container mx-auto px-4 py-8"] $ do
    div_ [class_ "bg-white rounded-lg shadow-md p-6"] $ do
      h1_ [class_ "text-3xl font-bold mb-6"] "Admin Dashboard"
      div_ [class_ "grid grid-cols-1 md:grid-cols-2 gap-6"] $ do
        div_ [class_ "bg-gray-50 p-4 rounded-lg"] $ do
          h2_ [class_ "text-xl font-semibold mb-2"] "RSVP Management"
          p_ [class_ "text-gray-600 mb-4"] "View and manage wedding RSVPs"
          a_ [href_ "/admin/rsvps", class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"] "View RSVPs"
        div_ [class_ "bg-gray-50 p-4 rounded-lg"] $ do
          h2_ [class_ "text-xl font-semibold mb-2"] "Settings"
          p_ [class_ "text-gray-600 mb-4"] "Manage website settings"
          button_ [class_ "bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded", disabled_ "disabled"] "Coming Soon"

-- | Handle login form submission
adminLoginHandler :: LoginForm -> AppM (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] (Html ()))
adminLoginHandler (LoginForm submittedPassword) = do
  adminPassword <- getAdminPassword
  if submittedPassword == adminPassword
    then do
      cookieSettings <- getCookieSettings
      jwtSettings <- getJWTSettings
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings AdminUser
      case mApplyCookies of
        Nothing -> throwError err401
        Just applyCookies -> do
          let loginSuccessHtml = div_ $ do
                h1_ "Login Successful"
                p_ "Redirecting to admin dashboard..."
                script_ "setTimeout(function(){ window.location.href = '/admin'; }, 2000);"
          return $ applyCookies loginSuccessHtml
    else throwError err401
