{-# LANGUAGE GADTs #-}
module GADT.Regular.GADT where

import Data.Text (Text)
import Data.Time.Calendar (Day)

data PatientInformation where
  PatientDetails :: Text -> Day -> PatientInformation -- initials and dob
  PatientId :: Int -> PatientInformation              -- id in the system