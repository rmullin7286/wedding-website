{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Wedding.Server where

import Control.Monad.IO.Class (liftIO)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.HTML.Lucid
import Lucid

import Wedding.Pages.Home (homePage)
import Wedding.Pages.Story (storyPage) 
import Wedding.Pages.Details (detailsPage)
import Wedding.Pages.RSVP (rsvpPage, rsvpFormHandler)
import Wedding.Types

-- | API definition
type WeddingAPI = 
       Get '[HTML] (Html ())
  :<|> "story" :> Get '[HTML] (Html ())
  :<|> "details" :> Get '[HTML] (Html ())
  :<|> "rsvp" :> Get '[HTML] (Html ())
  :<|> "rsvp" :> ReqBody '[JSON] RSVP :> Post '[HTML] (Html ())
  :<|> "static" :> Raw

-- | Server implementation
server :: Server WeddingAPI
server = return homePage
    :<|> return storyPage
    :<|> return detailsPage
    :<|> return rsvpPage
    :<|> rsvpFormHandler
    :<|> serveDirectoryWebApp "static"

-- | Wedding API proxy
weddingAPI :: Proxy WeddingAPI
weddingAPI = Proxy

-- | WAI Application
app :: Application
app = serve weddingAPI server

-- | Run the server
runServer :: IO ()
runServer = do
  putStrLn "Wedding website starting on http://localhost:8080"
  run 8080 app