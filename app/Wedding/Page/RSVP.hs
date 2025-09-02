{-# LANGUAGE RecordWildCards #-}

module Wedding.Page.RSVP (rsvpPage, rsvpNameSubmission, RSVPFormData) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Lucid (Html, action_, button_, class_, div_, for_, form_, h1_, id_, input_, label_, method_, name_, p_, placeholder_, required_, section_, type_)
import Network.HTTP.Types (hLocation)
import Servant (Handler, NoContent, ServerError (errHeaders), err302, throwError)
import Web.FormUrlEncoded (FromForm, fromForm, parseUnique)
import Wedding.Component.BasePage (basePage)

rsvpPage :: Html ()
rsvpPage = basePage "RSVP" $ do
  rsvpLanding

rsvpLanding :: Html ()
rsvpLanding = do
  section_ [class_ "py-5", id_ "rsvp-landing"] $ do
    div_ [class_ "container"] $ do
      div_ [class_ "row justify-content-center"] $ do
        div_ [class_ "col-lg-6 col-md-8 text-center"] $ do
          h1_ [class_ "display-4 mb-4"] "Please RSVP"
          p_ [class_ "lead mb-4"] "We're so excited to celebrate with you! Please enter your name below to get started."

          -- RSVP Form
          form_ [method_ "POST", action_ "/rsvp", class_ "rsvp-form"] $ do
            div_ [class_ "mb-4"] $ do
              label_ [for_ "guestName", class_ "form-label fs-5"] "Full Name"
              input_ [type_ "text", class_ "form-control form-control-lg", id_ "guestName", name_ "guestName", placeholder_ "Enter your full name", required_ ""]

            button_ [type_ "submit", class_ "btn btn-lg px-5"] "Continue"

-- | Form data for RSVP name submission
newtype RSVPFormData = RSVPFormData {guestName :: String}

instance FromForm RSVPFormData where
  fromForm f = RSVPFormData <$> parseUnique "guestName" f

rsvpNameSubmission :: RSVPFormData -> Handler NoContent
rsvpNameSubmission RSVPFormData {..} = do
  liftIO $ putStrLn $ "RSVP submission from: " ++ guestName
  throwError $ err302 {errHeaders = [(hLocation, "/")]}
