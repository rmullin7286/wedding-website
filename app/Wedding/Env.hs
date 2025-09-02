module Wedding.Env (Env, AppM, runAppM, mkEnv, getDbConnection) where

import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Database.SQLite.Simple (Connection)
import Servant (Handler)

-- | Global environment containing shared application state
-- Implementation details are hidden via newtype
newtype Env = Env
  { _envDbConnection :: Connection
  }

-- | Application monad with ReaderT for dependency injection
type AppM = ReaderT Env Handler

-- | Create a new environment with the provided database connection
mkEnv :: Connection -> Env
mkEnv = Env

-- | Get the database connection from the environment
getDbConnection :: AppM Connection
getDbConnection = asks _envDbConnection

-- | Run AppM with the provided environment
runAppM :: Env -> AppM a -> Handler a
runAppM env = flip runReaderT env
