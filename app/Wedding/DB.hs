{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Wedding.DB () where

import Data.Text (Text)

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
