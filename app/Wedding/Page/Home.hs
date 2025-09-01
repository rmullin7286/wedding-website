module Wedding.Page.Home (home) where

import Lucid (Html, class_, div_, h1_, id_, p_, section_)
import Wedding.Component.BasePage (basePage)

home :: Html ()
home = basePage "Home" $ do
  section_ [class_ "hero"] $ do
    div_ [class_ "hero-content container"] $ do
      h1_ [class_ "display-1 fw-bold"] "Ryan & Shae 2026"
      p_ [class_ "lead display-6 mb-4"] "Join us for our wedding celebration"
