{-# LANGUAGE RecordWildCards #-}

module Wedding.Page.RSVP (rsvpPage, rsvpNameSubmission, rsvpGroupSubmission, RSVPFormData, GroupRSVPFormData) where

import Control.Monad (forM_)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Effectful (Eff, (:>))
import Lucid (Html, action_, button_, class_, div_, for_, form_, h1_, h2_, id_, input_, label_, method_, name_, onclick_, p_, placeholder_, required_, rows_, section_, textarea_, toHtml, type_, value_)
import Web.FormUrlEncoded (FromForm, fromForm, parseAll, parseUnique)
import Wedding.Component.BasePage (basePage)
import Wedding.DB (Attendee (..), AttendingStatus (..), DB, getAllGroupMembersOfAttendeeNamed, updateAttendeeRSVP)

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
newtype RSVPFormData = RSVPFormData {guestName :: Text}

instance FromForm RSVPFormData where
  fromForm f = RSVPFormData <$> parseUnique "guestName" f

-- | Individual attendee RSVP data
data AttendeeRSVPData = AttendeeRSVPData
  { attendeeId :: Int,
    attending :: AttendingStatus,
    dietaryRestrictions :: Maybe Text
  }
  deriving (Show)

-- | Group RSVP form data containing all attendees
newtype GroupRSVPFormData = GroupRSVPFormData
  { attendeeRSVPs :: [AttendeeRSVPData]
  }
  deriving (Show)

instance FromForm GroupRSVPFormData where
  fromForm f = do
    attendeeIds <- parseAll "attendeeId" f
    attendees <- mapM (parseAttendeeData f) attendeeIds
    return $ GroupRSVPFormData attendees
    where
      parseAttendeeData form attendeeIdText = do
        let attendeeId = read $ T.unpack attendeeIdText
        let attendingFieldName = "attending_" <> attendeeIdText
        let dietaryFieldName = "dietary_" <> attendeeIdText

        attendingText <- parseUnique attendingFieldName form
        dietaryText <- parseUnique dietaryFieldName form

        let attendingStatus = read $ T.unpack attendingText
        let dietaryRestrictions = if T.null dietaryText then Nothing else Just dietaryText

        return $ AttendeeRSVPData attendeeId attendingStatus dietaryRestrictions

rsvpNameSubmission :: (DB :> es) => RSVPFormData -> Eff es (Html ())
rsvpNameSubmission RSVPFormData {..} = do
  attendees <- getAllGroupMembersOfAttendeeNamed guestName
  if null attendees
    then return $ guestNotFoundPage guestName
    else return $ groupRSVPPage attendees

-- | Group RSVP submission handler
rsvpGroupSubmission :: (DB :> es) => GroupRSVPFormData -> Eff es (Html ())
rsvpGroupSubmission (GroupRSVPFormData attendeeRSVPs) = do
  -- Process each attendee's RSVP data
  forM_ attendeeRSVPs $ \(AttendeeRSVPData aid attending dietary) -> do
    updateAttendeeRSVP aid attending dietary
  return rsvpSuccessPage

-- | Generate the group RSVP form page
groupRSVPPage :: [Attendee] -> Html ()
groupRSVPPage attendees = basePage "RSVP for Your Group" $ do
  section_ [class_ "py-5"] $ do
    div_ [class_ "container"] $ do
      div_ [class_ "row justify-content-center"] $ do
        div_ [class_ "col-lg-8"] $ do
          h1_ [class_ "display-5 mb-4 text-center"] "RSVP for Your Group"
          p_ [class_ "lead mb-5 text-center"] "Please fill out the RSVP information for everyone in your group."

          form_ [method_ "POST", action_ "/rsvp/submit", class_ "rsvp-group-form"] $ do
            forM_ attendees $ \attendee -> do
              attendeeRSVPForm attendee

            div_ [class_ "text-center mt-4"] $ do
              button_ [type_ "submit", class_ "btn btn-lg px-5"] "Submit RSVPs"

-- | Generate RSVP form for individual attendee
attendeeRSVPForm :: Attendee -> Html ()
attendeeRSVPForm (Attendee attendeeId attendeeName _ _ currentDietary) = do
  div_ [class_ "card mb-4"] $ do
    div_ [class_ "card-header"] $ do
      h2_ [class_ "h5 mb-0"] $ toHtml attendeeName
    div_ [class_ "card-body"] $ do
      -- Hidden field for attendee ID
      input_ [type_ "hidden", name_ "attendeeId", value_ $ pack $ show attendeeId]

      -- Attending status
      div_ [class_ "mb-3"] $ do
        label_ [class_ "form-label fw-bold"] "Will you be attending?"
        div_ [class_ ""] $ do
          attendingOption attendeeId "Yes" Yes
          attendingOption attendeeId "No" No
          attendingOption attendeeId "Maybe" Undecided

      -- Dietary restrictions
      div_ [class_ "mb-3"] $ do
        label_ [for_ $ "dietary_" <> pack (show attendeeId), class_ "form-label fw-bold"] "Dietary restrictions (optional)"
        textarea_
          [ name_ $ "dietary_" <> pack (show attendeeId),
            id_ $ "dietary_" <> pack (show attendeeId),
            class_ "form-control",
            rows_ "2",
            placeholder_ "Any food allergies or dietary requirements..."
          ]
          $ maybe "" toHtml currentDietary

-- | Generate radio button for attending status
attendingOption :: Int -> Text -> AttendingStatus -> Html ()
attendingOption attendeeId label status = do
  let fieldName = "attending_" <> pack (show attendeeId)
  let elementId = "attending_" <> pack (show attendeeId) <> "_" <> label
  div_ [class_ "form-check form-check-inline"] $ do
    input_
      [ class_ "form-check-input",
        type_ "radio",
        name_ fieldName,
        id_ elementId,
        value_ $ pack $ show status,
        required_ ""
      ]
    label_ [class_ "form-check-label", for_ elementId] $ toHtml label

-- | Guest not found error page
guestNotFoundPage :: Text -> Html ()
guestNotFoundPage guestName = basePage "Guest Not Found" $ do
  section_ [class_ "py-5"] $ do
    div_ [class_ "container text-center"] $ do
      h1_ [class_ "display-5 mb-4"] "Guest Not Found"
      p_ [class_ "lead mb-4"] $ do
        "We couldn't find \"" <> toHtml guestName <> "\" in our guest list."
      p_ "Please check the spelling of your name and try again, or contact us if you believe this is an error."
      div_ [class_ "mt-4"] $ do
        button_ [class_ "btn btn-primary", onclick_ "history.back()"] "Go Back"

-- | RSVP success page
rsvpSuccessPage :: Html ()
rsvpSuccessPage = basePage "RSVP Submitted" $ do
  section_ [class_ "py-5"] $ do
    div_ [class_ "container text-center"] $ do
      h1_ [class_ "display-5 mb-4"] "Thank You!"
      p_ [class_ "lead mb-4"] "Your RSVP has been submitted successfully."
      p_ "We're looking forward to celebrating with you!"
