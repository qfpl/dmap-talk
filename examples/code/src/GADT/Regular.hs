module GADT.Regular where

import Data.Text (Text)
import Data.Time.Calendar (Day)

data PatientInformation =
    PatientDetails Text Day -- initials and dob
  | PatientId Int           -- id in the system
  deriving (Eq, Ord, Show)
