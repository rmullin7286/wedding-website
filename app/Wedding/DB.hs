{-# LANGUAGE DuplicateRecordFields #-}

module Wedding.DB (initializeDatabase, Attendee (..), MonadDB (..), AttendingStatus (..), getAllGroupMembersOfAttendeeNamed) where

import Control.Lens ((^.))
import Control.Monad.RWS (MonadIO (liftIO))
import Data.Generics.Labels ()
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, FromRow, Only (Only), ToRow, execute, execute_, open, query, query_)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Wedding.Env (AppM, getDbConnection)

initializeDatabase :: FilePath -> IO Connection
initializeDatabase path = do
  -- Ensure parent directory exists
  createDirectoryIfMissing True (takeDirectory path)

  -- Open database (SQLite creates file if it doesn't exist)
  conn <- open path
  execute_ conn "PRAGMA foreign_keys = ON"
  execute_ conn "PRAGMA journal_mode = WAL"

  -- Create tables if they don't exist
  execute_ conn "CREATE TABLE IF NOT EXISTS attendees (id INTEGER PRIMARY KEY, name TEXT NOT NULL, group_name TEXT, attending TEXT, dietary_restrictions TEXT)"
  return conn

class (Monad m) => MonadDB m where
  getAttendeeByName :: Text -> m (Maybe Attendee)
  updateAttendeeGroup :: Int -> Maybe Text -> m ()
  createAttendee :: Text -> Maybe Text -> m ()
  getAllAttendees :: m [Attendee]
  getAttendeesByGroup :: Text -> m [Attendee]
  updateAttendeeRSVP :: Int -> AttendingStatus -> Maybe Text -> m ()

instance MonadDB AppM where
  getAttendeeByName name = do
    conn <- getDbConnection
    results <- liftIO $ query conn "SELECT * FROM attendees WHERE name=?" $ Only name
    return . listToMaybe $ results

  updateAttendeeGroup aid group = do
    conn <- getDbConnection
    liftIO $ execute conn "UPDATE attendees SET group_name = ? WHERE id = ?" (group, aid)

  createAttendee name group = do
    conn <- getDbConnection
    liftIO $ execute conn "INSERT INTO attendees (name, group_name, attending) VALUES(?, ?, ?)" (name, group, Undecided)

  getAllAttendees = do
    conn <- getDbConnection
    liftIO $ query_ conn "SELECT * FROM attendees"

  getAttendeesByGroup group = do
    conn <- getDbConnection
    liftIO $ query conn "SELECT * FROM attendees WHERE group_name = ?" $ Only group

  updateAttendeeRSVP attendeeId attending dietary = do
    conn <- getDbConnection
    liftIO $ execute conn "UPDATE attendees SET attending = ?, dietary_restrictions = ? WHERE id = ?" (attending, dietary, attendeeId)

getAllGroupMembersOfAttendeeNamed :: (MonadDB m) => Text -> m [Attendee]
getAllGroupMembersOfAttendeeNamed name = do
  attendee <- getAttendeeByName name
  case attendee of
    Just a -> case a ^. #group of
      Just group -> getAttendeesByGroup group
      Nothing -> return [a]
    Nothing -> return []

newtype ViaShow a = ViaShow a

instance (Show a) => ToField (ViaShow a) where
  toField (ViaShow x) = toField (show x)

instance (Read a) => FromField (ViaShow a) where
  fromField f = ViaShow . read <$> fromField f

data AttendingStatus = Yes | No | Undecided
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromField, ToField) via (ViaShow AttendingStatus)

data Attendee = Attendee
  { id :: Int,
    name :: Text,
    group :: Maybe Text,
    attending :: AttendingStatus,
    dietaryRestrictions :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow)
