module Wedding.Env (Env (..), AppM, runAppM) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Database.SQLite.Simple (Connection)
import Servant (Handler)

-- | Global environment containing shared application state
data Env = Env
  { envDbConnection :: Connection
  }

-- | Application monad with ReaderT for dependency injection
type AppM = ReaderT Env Handler

-- | Run AppM with the provided environment
runAppM :: Env -> AppM a -> Handler a
runAppM env = flip runReaderT env
