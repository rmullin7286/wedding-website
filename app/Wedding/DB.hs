{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Wedding.DB (initializeDatabase) where

import Data.Text (Text)
import Database.SQLite.Simple (Connection, execute_, open)

initializeDatabase :: FilePath -> IO Connection
initializeDatabase path = do
  conn <- open path
  execute_ conn "PRAGMA foreign_keys = ON"
  execute_ conn "PRAGMA journal_mode = WAL"

  -- Create tables if they don't exist
  execute_ conn "CREATE TABLE IF NOT EXISTS groups (id INTEGER PRIMARY KEY, name TEXT NOT NULL)"
  execute_ conn "CREATE TABLE IF NOT EXISTS attendees (id INTEGER PRIMARY KEY, group_id INTEGER REFERENCES groups(id), name TEXT NOT NULL, attending BOOLEAN, dietary_restrictions TEXT)"
  return conn

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
