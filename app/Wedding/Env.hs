module Wedding.Env (Env, AppM, runAppM, mkEnv, getDbConnection, liftIO, ask, throwError) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Database.SQLite.Simple (Connection)
import Servant (Handler, ServerError, throwError)

-- | Global environment containing shared application state
-- Implementation details are hidden via newtype
newtype Env = Env
  { _envDbConnection :: Connection
  }

-- | Application monad with ReaderT for dependency injection
-- Implementation details are hidden via newtype
newtype AppM a = AppM {unAppM :: ReaderT Env Handler a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError ServerError)

-- | Create a new environment with the provided database connection
mkEnv :: Connection -> Env
mkEnv = Env

-- | Get the database connection from the environment
getDbConnection :: AppM Connection
getDbConnection = asks _envDbConnection

-- | Run AppM with the provided environment
runAppM :: Env -> AppM a -> Handler a
runAppM env appM = runReaderT (unAppM appM) env
