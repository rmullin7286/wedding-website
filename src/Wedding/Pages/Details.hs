module Wedding.Pages.Details (detailsPage) where

import Lucid
import Wedding.Pages.Home (baseTemplate)

-- | Wedding Details page
detailsPage :: Html ()
detailsPage = baseTemplate "Wedding Details" $ do
  section_ [class_ "details-hero"] $ do
    h1_ "Wedding Details"
    p_ [class_ "details-subtitle"] "Everything you need to know about our special day"
  
  section_ [class_ "schedule"] $ do
    div_ [class_ "section-header"] $ do
      h2_ $ do
        span_ [class_ "section-icon"] "⏰"
        "Schedule of Events"
    div_ [class_ "schedule-timeline"] $ do
      scheduleItem "🚗" "3:30 PM" "Guest Arrival" "Please arrive 30 minutes before the ceremony"
      scheduleItem "💍" "4:00 PM" "Ceremony" "Exchange of vows surrounded by beautiful gardens"
      scheduleItem "🥂" "4:30 PM" "Cocktail Hour" "Drinks and appetizers among the flowers"
      scheduleItem "🍽️" "6:00 PM" "Reception" "Dinner, dancing, and celebration"
      scheduleItem "✨" "11:00 PM" "Send Off" "Sparkler farewell under the stars"

  section_ [class_ "venue"] $ do
    div_ [class_ "section-header"] $ do
      h2_ $ do
        span_ [class_ "section-icon"] "📍"
        "Venue Information"
    div_ [class_ "venue-card"] $ do
      div_ [class_ "venue-main"] $ do
        h3_ [class_ "venue-name"] "Trillium Nursery"
        p_ [class_ "venue-description"] "A charming nursery venue surrounded by beautiful gardens and lush greenery - the perfect setting for our garden party wedding!"
        div_ [class_ "venue-details-grid"] $ do
          div_ [class_ "venue-detail"] $ do
            span_ [class_ "detail-icon"] "📍"
            div_ $ do
              strong_ "Address"
              p_ "15001 Union Ave SE, Renton, WA 98059"
          div_ [class_ "venue-detail"] $ do
            span_ [class_ "detail-icon"] "📞"
            div_ $ do
              strong_ "Phone"
              p_ "(425) 555-0123"
          div_ [class_ "venue-detail"] $ do
            span_ [class_ "detail-icon"] "🌐"
            div_ $ do
              strong_ "Website"
              p_ $ a_ [href_ "https://trilliumnursery.com", target_ "_blank"] "trilliumnursery.com"
      div_ [class_ "venue-map-placeholder"] $ do
        div_ [class_ "map-icon"] "🗺️"
        p_ "Interactive map coming soon"
        p_ [class_ "directions-note"] "Easy access from I-405 and Highway 167"

  section_ [class_ "logistics"] $ do
    div_ [class_ "section-header"] $ do
      h2_ $ do
        span_ [class_ "section-icon"] "ℹ️"
        "Important Information"
    div_ [class_ "info-cards-grid"] $ do
      infoCard "👗" "Dress Code" $ do
        p_ [class_ "info-highlight"] "Semi-formal / Garden Party Attire"
        ul_ [class_ "info-list"] $ do
          li_ "Ladies: Cocktail dresses, dressy separates, floral prints welcome"
          li_ "Gentlemen: Dress shirt and slacks, jacket optional"
          li_ "Colors: Earth tones and garden colors encouraged"
        p_ [class_ "info-note"] "👠 Note: Ceremony is outdoors on grass - choose footwear accordingly!"
      
      infoCard "🚗" "Transportation & Parking" $ do
        ul_ [class_ "info-list"] $ do
          li_ "✅ Free parking available on-site"
          li_ "🅿️ Ample spaces for all guests"
          li_ "🛣️ Easy access from I-405 and Highway 167"
          li_ "📍 GPS-friendly location with clear signage"
      
      infoCard "🎁" "Wedding Registry" $ do
        p_ [class_ "info-highlight"] "Your presence is the greatest gift!"
        p_ "If you wish to give a gift, we're registered at:"
        div_ [class_ "registry-links"] $ do
          a_ [href_ "#", class_ "registry-button"] "Williams Sonoma"
          a_ [href_ "#", class_ "registry-button"] "Crate & Barrel"
          a_ [href_ "#", class_ "registry-button"] "Zola Registry"

-- | Schedule item component with icon
scheduleItem :: Html () -> Html () -> Html () -> Html () -> Html ()
scheduleItem icon time event description = div_ [class_ "schedule-item"] $ do
  div_ [class_ "schedule-icon"] icon
  div_ [class_ "schedule-time"] time
  div_ [class_ "schedule-content"] $ do
    h3_ event
    p_ description

-- | Info card component for logistics section
infoCard :: Html () -> Html () -> Html () -> Html ()
infoCard icon title content = div_ [class_ "info-card-details"] $ do
  div_ [class_ "info-card-header"] $ do
    span_ [class_ "info-card-icon"] icon
    h3_ title
  div_ [class_ "info-card-content"] content