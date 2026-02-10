module Wedding.Page.Home (home) where

import Lucid (Html, a_, alt_, class_, div_, h1_, h2_, height_, href_, id_, img_, p_, section_, src_, target_, width_)
import Wedding.Component.BasePage (basePage)

home :: Html ()
home = basePage "Home" $ do
  hero
  whenAndWhere
  rsvpSection
  registrySection
  contactSection

hero :: Html ()
hero = do
  section_ [class_ "hero"] $ do
    div_ [class_ "hero-content container"] $ do
      h1_ [class_ "display-1 fw-bold"] "Ryan & Shae"
      p_ [class_ "lead display-6 mb-4"] "May 30, 2026"
      p_ [class_ "lead"] "Join us for our wedding"

whenAndWhere :: Html ()
whenAndWhere = do
  section_ [class_ "py-5 bg-light", id_ "when-where"] $ do
    div_ [class_ "container"] $ do
      div_ [class_ "row g-5"] $ do
        -- When Column
        div_ [class_ "col-md-6 text-center py-4"] $ do
          h2_ [class_ "h2 mb-4"] "When"
          p_ [class_ "mb-3 fs-3"] "Saturday, May 30th, 2026"
          p_ [class_ "text-muted fs-5 mb-1"] "Guests arrive at 2:30 PM"
          p_ [class_ "text-muted fs-5 mb-1"] "Ceremony starts promptly at 3 PM"
          p_ [class_ "text-muted fs-5"] "Reception to follow"

        -- Where Column
        div_ [class_ "col-md-6 text-center py-4"] $ do
          h2_ [class_ "h2 mb-4"] "Where"
          p_ [class_ "mb-3 fs-3"] "Trillium Nursery"
          p_ [class_ "text-muted fs-5 mb-3"] "8335 196th Ave NE, Redmond, WA 98053"
          a_ [href_ "https://maps.app.goo.gl/Hxt1t2ABHH3z4Wtz6", target_ "_blank", class_ "btn btn-outline-light"] "View on Google Maps"

rsvpSection :: Html ()
rsvpSection = do
  section_ [class_ "py-5", id_ "rsvp"] $ do
    div_ [class_ "container"] $ do
      div_ [class_ "row justify-content-center"] $ do
        div_ [class_ "col-lg-8 text-center py-4"] $ do
          h1_ [class_ "display-5 mb-4"] "Join Us"
          p_ [class_ "fs-4 mb-4"] "We hope you'll join us on this special day as we celebrate our love and begin our journey together."
          a_ [href_ "/rsvp", class_ "btn btn-lg px-5"] "RSVP"

registrySection :: Html ()
registrySection = do
  section_ [class_ "py-5", id_ "registry"] $ do
    div_ [class_ "container"] $ do
      div_ [class_ "row justify-content-center"] $ do
        div_ [class_ "col-lg-8 text-center py-4"] $ do
          h1_ [class_ "display-5 mb-4"] "Registry"
          p_ [class_ "lead fs-4 mb-4"] "Your presence at our wedding is the greatest gift of all. If you wish to honor us with a gift, we have a registry at Honeyfund."
          img_ [src_ "/static/image/registry-qr.png", alt_ "Registry QR Code", class_ "registry-qr mb-4", width_ "200", height_ "200"]
          div_ [] $ do
            a_ [href_ "https://www.honeyfund.com/site/mullin-huot-05-30-2026", target_ "_blank", class_ "btn btn-lg px-5"] "View Registry"

contactSection :: Html ()
contactSection = do
  section_ [class_ "py-5", id_ "contact"] $ do
    div_ [class_ "container"] $ do
      div_ [class_ "row justify-content-center mb-5"] $ do
        div_ [class_ "col-lg-8 text-center"] $ do
          h1_ [class_ "display-5 mb-4"] "Contact Us"
          p_ [class_ "lead"] "Feel free to reach out if you have any questions."

      div_ [class_ "row justify-content-center"] $ do
        div_ [class_ "col-lg-6"] $ do
          div_ [class_ "contact-table"] $ do
            -- Email Entry
            div_ [class_ "contact-row"] $ do
              p_ [class_ "contact-label fs-4"] "Email"
              a_ [href_ "mailto:ryanandshaemullin@gmail.com", class_ "contact-link fs-3"] "ryanandshaemullin@gmail.com"

            -- Phone Entry
            div_ [class_ "contact-row"] $ do
              p_ [class_ "contact-label fs-4"] "Phone"
              a_ [href_ "tel:+14258791509", class_ "contact-link fs-3"] "425-879-1509"
