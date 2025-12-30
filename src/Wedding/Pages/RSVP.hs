module Wedding.Pages.RSVP (rsvpPage, rsvpFormHandler) where

import Lucid
import Lucid.Base (makeAttribute)
import Control.Monad.IO.Class (liftIO)
import Servant
import Wedding.Pages.Home (baseTemplate)
import Wedding.Types
import Data.Text (Text)

-- | RSVP page with form
rsvpPage :: Html ()
rsvpPage = baseTemplate "RSVP" $ do
  section_ [class_ "rsvp-hero"] $ do
    h1_ "RSVP"
    p_ [class_ "rsvp-subtitle"] "Please let us know if you can celebrate with us!"
    p_ [class_ "rsvp-deadline"] "Please respond by April 1st, 2026"
  
  section_ [class_ "rsvp-form-section"] $ do
    div_ [class_ "form-container"] $
      form_ [hxPost_ "/rsvp", hxTarget_ "#rsvp-response", class_ "rsvp-form"] $ do
        div_ [class_ "form-group"] $ do
          label_ [for_ "name"] "Your Name *"
          input_ [type_ "text", id_ "name", name_ "name", required_ "required"]
        
        div_ [class_ "form-group"] $ do
          label_ [for_ "email"] "Email Address *"
          input_ [type_ "email", id_ "email", name_ "email", required_ "required"]
        
        div_ [class_ "form-group"] $ do
          label_ [class_ "radio-label"] $ do
            input_ [type_ "radio", name_ "attending", value_ "yes", id_ "attending-yes"]
            "Joyfully accepts"
          label_ [class_ "radio-label"] $ do
            input_ [type_ "radio", name_ "attending", value_ "no", id_ "attending-no"]
            "Regretfully declines"
        
        div_ [id_ "attending-details", class_ "form-section"] $ do
          div_ [class_ "form-group"] $ do
            label_ [class_ "checkbox-label"] $ do
              input_ [type_ "checkbox", name_ "plus-one", id_ "plus-one"]
              "I will be bringing a plus one"
          
          div_ [id_ "plus-one-details", class_ "form-group", style_ "display: none;"] $ do
            label_ [for_ "plus-one-name"] "Plus One Name"
            input_ [type_ "text", id_ "plus-one-name", name_ "plus-one-name"]
          
          div_ [class_ "form-group"] $ do
            label_ [for_ "meal-choice"] "Meal Choice *"
            select_ [id_ "meal-choice", name_ "meal-choice"] $ do
              option_ [value_ ""] "Please select..."
              option_ [value_ "chicken"] "Herb Roasted Chicken"
              option_ [value_ "fish"] "Pan-Seared Salmon"
              option_ [value_ "beef"] "Grilled Beef Tenderloin"
              option_ [value_ "vegetarian"] "Vegetarian Pasta Primavera"
          
          div_ [id_ "plus-one-meal", class_ "form-group", style_ "display: none;"] $ do
            label_ [for_ "plus-one-meal-choice"] "Plus One Meal Choice"
            select_ [id_ "plus-one-meal-choice", name_ "plus-one-meal-choice"] $ do
              option_ [value_ ""] "Please select..."
              option_ [value_ "chicken"] "Herb Roasted Chicken"
              option_ [value_ "fish"] "Pan-Seared Salmon"
              option_ [value_ "beef"] "Grilled Beef Tenderloin"
              option_ [value_ "vegetarian"] "Vegetarian Pasta Primavera"
        
        div_ [class_ "form-group"] $ do
          label_ [for_ "message"] "Special Message (Optional)"
          textarea_ [id_ "message", name_ "message", rows_ "4", 
                    placeholder_ "Share your excitement, dietary restrictions, or special requests..."] ""
        
        button_ [type_ "submit", class_ "submit-btn"] "Send RSVP"
    
    div_ [id_ "rsvp-response", class_ "response-area"] ""

-- | Handle RSVP form submission
rsvpFormHandler :: RSVP -> Handler (Html ())
rsvpFormHandler rsvp = do
  -- TODO: Save to database
  liftIO $ print rsvp  -- For now, just print to console
  return $ div_ [class_ "success-message"] $ do
    h3_ "Thank you for your RSVP!"
    p_ $ "We've received your response, " <> toHtml (rsvpName rsvp) <> "."
    if rsvpAttending rsvp
      then p_ "We're so excited to celebrate with you!"
      else p_ "We'll miss you on our special day, but understand."
    div_ [class_ "mt-4"] $ do
      a_ [href_ "/", class_ "btn btn-primary btn-lg px-5"] "Return to Home"

-- | Custom HTMX attributes
hxPost_ :: Text -> Attribute
hxPost_ = makeAttribute "hx-post"

hxTarget_ :: Text -> Attribute  
hxTarget_ = makeAttribute "hx-target"