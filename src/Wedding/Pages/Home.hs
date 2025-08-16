module Wedding.Pages.Home (homePage, baseTemplate) where

import Lucid
import Data.Text (Text)

-- | Home page with hero section
homePage :: Html ()
homePage = baseTemplate "Home" $ do
  hero
  quickInfo
  navigationSection

-- | Hero section with main wedding details
hero :: Html ()
hero = section_ [class_ "hero"] $ do
  div_ [class_ "hero-content"] $ do
    h1_ [class_ "couple-names"] "Ryan & Shae"
    p_ [class_ "wedding-date"] "May 30th, 2026"
    p_ [class_ "wedding-location"] "Trillium Nursery, Redmond WA"
    div_ [class_ "hero-image"] $
      img_ [src_ "/static/images/Ryan+Shae.jpg", alt_ "Ryan and Shae"]

-- | Quick wedding information
quickInfo :: Html ()
quickInfo = section_ [class_ "quick-info"] $ do
  h2_ "Join us for our special day"
  div_ [class_ "info-grid"] $ do
    infoCard "📅" "Date" "Friday, May 30th, 2026"
    infoCard "🕐" "Time" "4:00 PM Ceremony"
    infoCard "📍" "Location" "Trillium Nursery, Redmond WA"
    infoCard "🎉" "Reception" "Following ceremony"

-- | Info card component
infoCard :: Html () -> Html () -> Html () -> Html ()
infoCard icon title content = div_ [class_ "info-card"] $ do
  div_ [class_ "info-icon"] icon
  h3_ title
  p_ content

-- | Navigation to other sections
navigationSection :: Html ()
navigationSection = section_ [class_ "navigation"] $ do
  h2_ "Explore"
  div_ [class_ "nav-grid"] $ do
    navCard "/story" "Our Story" "How we met and fell in love"
    navCard "/details" "Wedding Details" "Venue, schedule, and logistics"
    navCard "/rsvp" "RSVP" "Let us know if you can make it"

-- | Navigation card component
navCard :: Text -> Html () -> Html () -> Html ()
navCard url title description = a_ [href_ url, class_ "nav-card"] $ do
  h3_ title
  p_ description

-- | Base HTML template
baseTemplate :: Html () -> Html () -> Html ()
baseTemplate pageTitle content = html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ $ "Ryan & Shae's Wedding - " <> pageTitle
    link_ [rel_ "stylesheet", href_ "/static/css/style.css?v=2"]
    script_ [src_ "https://unpkg.com/htmx.org@1.9.6"] ("" :: Text)
  body_ $ do
    header_ $ nav_ [class_ "main-nav"] $ do
      a_ [href_ "/", class_ "nav-brand"] "R & S"
      ul_ [class_ "nav-links"] $ do
        li_ $ a_ [href_ "/"] "Home"
        li_ $ a_ [href_ "/story"] "Our Story"
        li_ $ a_ [href_ "/details"] "Details"
        li_ $ a_ [href_ "/rsvp"] "RSVP"
    main_ [class_ "main-content"] content
    footer_ [class_ "main-footer"] $ do
      p_ "Ryan & Shae's Wedding 2026"
      p_ "Made with ❤️ and Haskell"
