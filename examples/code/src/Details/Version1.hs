module Details.Version1 where

import Data.Text (Text)
import Data.Time.Calendar (Day)

import Details.Common

data Details =
  Details {
    dId :: Id
  , dInitials :: Text
  , dDOB :: Day
  , dWeight :: Maybe Weight
  } deriving (Eq, Ord, Show)
