module Wedding.Env
  ( Env (..),
    AppM,
    runAppM,
    getDbConnection,
    getAdminPassword,
    getJWTSettings,
    getCookieSettings,
    liftIO,
    ask,
    throwError,
    loadEnv,
  )
where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection)
import Servant (Handler, ServerError, throwError)
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings, generateKey)
import System.Environment (getEnv)
import Wedding.DB (initializeDatabase)

-- | Global environment containing shared application state
-- Implementation details are hidden via newtype
data Env = Env
  { _envDbConnection :: Connection,
    _envAdminPassword :: Text,
    _envJWTSettings :: JWTSettings,
    _envCookieSettings :: CookieSettings
  }

-- | Method to load environment from the system.
-- Should be called at the beginning of the program before the server starts.
loadEnv :: IO Env
loadEnv =
  Env
    <$> (getEnv "WEDDING_DATABASE" >>= initializeDatabase)
    <*> (T.pack <$> getEnv "WEDDING_PASSWORD")
    <*> (defaultJWTSettings <$> generateKey)
    <*> pure defaultCookieSettings

-- | Application monad with ReaderT for dependency injection
-- Implementation details are hidden via newtype
newtype AppM a = AppM {unAppM :: ReaderT Env Handler a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError ServerError)

-- | Get the database connection from the environment
getDbConnection :: AppM Connection
getDbConnection = asks _envDbConnection

-- | Get the admin password from the environment
getAdminPassword :: AppM Text
getAdminPassword = asks _envAdminPassword

-- | Get JWT settings from the environment
getJWTSettings :: AppM JWTSettings
getJWTSettings = asks _envJWTSettings

-- | Get cookie settings from the environment
getCookieSettings :: AppM CookieSettings
getCookieSettings = asks _envCookieSettings

-- | Run AppM with the provided environment
runAppM :: Env -> AppM a -> Handler a
runAppM env appM = runReaderT (unAppM appM) env
