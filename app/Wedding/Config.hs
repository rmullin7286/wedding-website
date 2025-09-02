{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Wedding.Config (Config (..), DBConfig (..), loadConfig) where

import Data.Yaml (FromJSON, decodeFileThrow)
import GHC.Generics (Generic)

loadConfig :: FilePath -> IO Config
loadConfig = decodeFileThrow

data Config = Config
  { database :: DBConfig
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data DBConfig = DBConfig
  { location :: FilePath
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)
