{-# LANGUAGE DeriveGeneric #-}

module Wedding.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Guest RSVP information
data RSVP = RSVP
  { rsvpId :: Maybe Int
  , rsvpName :: Text
  , rsvpEmail :: Text
  , rsvpAttending :: Bool
  , rsvpPlusOne :: Bool
  , rsvpPlusOneName :: Maybe Text
  , rsvpMealChoice :: MealChoice
  , rsvpPlusOneMeal :: Maybe MealChoice
  , rsvpMessage :: Maybe Text
  , rsvpSubmittedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

instance FromJSON RSVP
instance ToJSON RSVP

-- | Meal choice options
data MealChoice
  = Vegetarian
  | Chicken
  | Fish
  | Beef
  deriving (Show, Eq, Generic)

instance FromJSON MealChoice
instance ToJSON MealChoice

-- | Wedding event information
data WeddingEvent = WeddingEvent
  { eventName :: Text
  , eventTime :: Text
  , eventLocation :: Text
  , eventDescription :: Maybe Text
  } deriving (Show, Eq, Generic)

-- | Contact information
data ContactInfo = ContactInfo
  { contactName :: Text
  , contactRole :: Text
  , contactPhone :: Maybe Text
  , contactEmail :: Maybe Text
  } deriving (Show, Eq, Generic)