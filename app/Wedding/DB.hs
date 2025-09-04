{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Wedding.DB (initializeDatabase) where

import Control.Monad.RWS (MonadReader)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, execute_, open)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

initializeDatabase :: FilePath -> IO Connection
initializeDatabase path = do
  -- Ensure parent directory exists
  createDirectoryIfMissing True (takeDirectory path)

  -- Open database (SQLite creates file if it doesn't exist)
  conn <- open path
  execute_ conn "PRAGMA foreign_keys = ON"
  execute_ conn "PRAGMA journal_mode = WAL"

  -- Create tables if they don't exist
  execute_ conn "CREATE TABLE IF NOT EXISTS groups (id INTEGER PRIMARY KEY, name TEXT NOT NULL)"
  execute_ conn "CREATE TABLE IF NOT EXISTS attendees (id INTEGER PRIMARY KEY, group_id INTEGER REFERENCES groups(id), name TEXT NOT NULL, attending BOOLEAN, dietary_restrictions TEXT)"
  return conn

class HasConnection a where
  getConnection :: a -> Connection

class (Monad m) => MonadDB m

instance (HasConnection env, MonadReader env m) => MonadDB m

data Attendee = Attendee
  { id :: Int,
    groupId :: Int,
    name :: Text,
    attending :: Bool,
    dietaryRestrictions :: Bool
  }

data Group = Group
  { id :: Int,
    name :: Text
  }
