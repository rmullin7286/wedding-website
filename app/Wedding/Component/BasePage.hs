{-# LANGUAGE TemplateHaskell #-}

module Wedding.Component.BasePage (basePage) where

import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Language.Haskell.TH.Syntax (lift, runIO)
import Lucid (Html, With (with), body_, charset_, content_, crossorigin_, doctype_, head_, href_, html_, integrity_, lang_, link_, meta_, name_, rel_, script_, src_, title_)
import System.Environment (lookupEnv)
import Wedding.Component.NavBar (navBar)

cssVersion :: String
cssVersion = $(runIO (fromMaybe "unknown" <$> lookupEnv "GIT_HASH") >>= lift)

basePage :: Html () -> Html () -> Html ()
basePage title content = do
  doctype_
  html_ [lang_ "en"] $ do
    head_ $ do
      -- required meta tags
      meta_ [charset_ "utf8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]

      -- Bootstrap import
      link_ [href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css", rel_ "stylesheet", integrity_ "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC", crossorigin_ "anonymous"]

      -- Page title
      title_ $ "Ryan & Shae's Wedding - " <> title

      -- Website style
      link_ [rel_ "stylesheet", href_ $ "/static/style.css?v=" <> pack cssVersion]

    body_ $ do
      -- Navigation bar
      navBar

      -- insert content
      content

      -- Import JS stuff
      with (script_ mempty) [src_ "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/js/bootstrap.bundle.min.js", integrity_ "sha384-IQsoLXl5PILFhosVNubq5LC7Qb9DXgDA9i+tQ8Zj3iwWAwPtgFTxbJ8NT4GN1R8p", crossorigin_ "anonymous"]

      with (script_ mempty) [src_ "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/js/bootstrap.min.js", integrity_ "sha384-cVKIPhGWiC2Al4u+LWgxfKTRIcfu0JTxR+EQDz/bgldoEyl4H0zUF0QKbrJ0EcQF", crossorigin_ "anonymous"]
