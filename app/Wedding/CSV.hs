module Wedding.CSV (insertFromCsv, GuestCSVRow(..)) where

import Control.Lens ((^.))
import Control.Monad (forM_)
import Data.Csv (FromRecord)
import Data.Generics.Labels ()
import Data.Text (Text)
import GHC.Generics (Generic)
import Wedding.DB (MonadDB (..))

data GuestCSVRow = GuestCSVRow
  { name :: Text,
    group :: Maybe Text
  }
  deriving (Generic, Show)

instance FromRecord GuestCSVRow

insertFromCsv :: (MonadDB m) => [GuestCSVRow] -> m ()
insertFromCsv rows = forM_ rows $ \row -> do
  existing <- getAttendeeByName $ row ^. #name
  case existing of
    Just existing' -> updateAttendeeGroup (existing' ^. #id) (row ^. #group)
    Nothing -> createAttendee (row ^. #name) (row ^. #group)
