{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Wedding.DB
  ( AttendingStatus (..),
    Attendee (..),
    DB,
    getAttendeeByName,
    getAttendeeById,
    updateAttendeeGroup,
    createAttendee,
    getAllAttendees,
    getAttendeesByGroup,
    updateAttendeeRSVP,
    updateAttendee,
    deleteAttendee,
    getAllGroupMembersOfAttendeeNamed,
    initializeDatabase,
    runDBIO,
  )
where

import Control.Lens ((^.))
import Data.Generics.Labels ()
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, FromRow, Only (Only), ToRow, execute, execute_, open, query, query_)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Effectful (Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpretWith)
import Effectful.TH (makeEffect)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

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

data DB :: Effect where
  GetAttendeeByName :: Text -> DB m (Maybe Attendee)
  GetAttendeeById :: Int -> DB m (Maybe Attendee)
  UpdateAttendeeGroup :: Int -> Maybe Text -> DB m ()
  CreateAttendee :: Text -> Maybe Text -> DB m ()
  GetAllAttendees :: DB m [Attendee]
  GetAttendeesByGroup :: Text -> DB m [Attendee]
  UpdateAttendeeRSVP :: Int -> AttendingStatus -> Maybe Text -> DB m ()
  UpdateAttendee :: Attendee -> DB m ()
  DeleteAttendee :: Int -> DB m ()

makeEffect ''DB

runDBIO :: (IOE :> es) => Connection -> Eff (DB : es) a -> Eff es a
runDBIO conn action = interpretWith action $ \_ -> \case
  GetAttendeeByName name -> liftIO $ listToMaybe <$> query conn "SELECT * FROM attendees WHERE name=?" (Only name)
  GetAttendeeById attendeeId -> liftIO $ listToMaybe <$> query conn "SELECT * FROM attendees WHERE id=?" (Only attendeeId)
  UpdateAttendeeGroup aid group -> liftIO $ execute conn "UPDATE attendees SET group_name = ? WHERE id = ?" (group, aid)
  CreateAttendee name group -> liftIO $ execute conn "INSERT INTO attendees(name, group_name, attending) VALUES(?, ?, ?)" (name, group, Undecided)
  GetAllAttendees -> liftIO $ query_ conn "SELECT * FROM attendees"
  GetAttendeesByGroup group -> liftIO $ query conn "SELECT * FROM attendees WHERE group_name = ?" $ Only group
  UpdateAttendeeRSVP attendeeId attending dietary -> liftIO $ execute conn "UPDATE attendees SET attending= ?, dietary_restrictions = ? WHERE id = ?" (attending, dietary, attendeeId)
  UpdateAttendee (Attendee attendeeId name group attending dietary) -> liftIO $ execute conn "UPDATE attendees SET name = ?, group_name = ?, attending = ?, dietary_restrictions = ? WHERE id = ?" (name, group, attending, dietary, attendeeId)
  DeleteAttendee attendeeId -> liftIO $ execute conn "DELETE FROM attendees WHERE id = ?" (Only attendeeId)

initializeDatabase :: FilePath -> IO Connection
initializeDatabase path = do
  -- Ensure parent directory exists
  createDirectoryIfMissing True (System.FilePath.takeDirectory path)

  -- Open database (SQLite creates file if it doesn't exist)
  conn <- open path
  execute_ conn "PRAGMA foreign_keys = ON"
  execute_ conn "PRAGMA journal_mode = WAL"

  -- Create tables if they don't exist
  execute_ conn "CREATE TABLE IF NOT EXISTS attendees (id INTEGER PRIMARY KEY, name TEXT NOT NULL, group_name TEXT, attending TEXT, dietary_restrictions TEXT)"
  return conn

getAllGroupMembersOfAttendeeNamed :: (DB :> es) => Text -> Eff es [Attendee]
getAllGroupMembersOfAttendeeNamed name = do
  attendee <- getAttendeeByName name
  case attendee of
    Nothing -> return []
    Just a -> case a ^. #group of
      Nothing -> return [a]
      Just g -> getAttendeesByGroup g
