module Wedding.Component.NavBar (navBar) where

import Lucid (Html, a_, button_, class_, div_, href_, id_, li_, nav_, span_, type_, ul_)
import Wedding.Html.Attributes (ariaControls_, ariaExpanded_, ariaLabel_, dataBsTarget_, dataBsToggle_)

navBar :: Html ()
navBar = nav_ [class_ "navbar navbar-expand-lg navbar-light wedding-navbar"] $ do
  div_ [class_ "container"] $ do
    -- Brand
    a_ [class_ "navbar-brand", href_ "/"] $ do
      span_ [class_ "brand-text"] "Ryan & Shae"

    -- Mobile toggle button
    button_ [class_ "navbar-toggler", type_ "button", dataBsToggle_ "collapse", dataBsTarget_ "#navbarNav", ariaControls_ "navbarNav", ariaExpanded_ "false", ariaLabel_ "Toggle navigation"] $ do
      span_ [class_ "navbar-toggler-icon"] ""

    -- Navigation links
    div_ [class_ "collapse navbar-collapse", id_ "navbarNav"] $ do
      ul_ [class_ "navbar-nav ms-auto"] $ do
        li_ [class_ "nav-item"] $ do
          a_ [class_ "nav-link", href_ "/"] "Home"
        li_ [class_ "nav-item"] $ do
          a_ [class_ "nav-link", href_ "#when-where"] "When & Where"
        li_ [class_ "nav-item"] $ do
          a_ [class_ "nav-link", href_ "/rsvp"] "RSVP"
