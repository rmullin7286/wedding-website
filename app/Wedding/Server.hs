{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Wedding.Server (runServer) where

import Lucid (Html)
import Network.Wai.Handler.Warp (run)
import Servant (Application, Proxy (Proxy), Server, serve, serveDirectoryWebApp, (:<|>) (..))
import Servant.API (Get, Raw, (:>))
import Servant.HTML.Lucid (HTML)
import Wedding.Page.Home (home)

-- | API Definition
type WeddingAPI =
  Get '[HTML] (Html ())
    :<|> "static" :> Raw

server :: Server WeddingAPI
server =
  return home
    :<|> serveDirectoryWebApp "static"

weddingAPI :: Proxy WeddingAPI
weddingAPI = Proxy

app :: Application
app = serve weddingAPI server

runServer :: IO ()
runServer = run 8080 app
