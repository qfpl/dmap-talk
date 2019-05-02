{-# LANGUAGE GADTs #-}
module GADT.Existential.GADT where

import Data.Text (Text)
import Data.Time.Calendar (Day)

data PatientInformation where
  PatientDetails :: Show a => Text -> Day -> a -> PatientInformation -- initials and dob and other
  PatientId :: Int -> PatientInformation                             -- id in the system

instance Show PatientInformation where
  showsPrec n (PatientId ident) =
    showString "PatientId " .
    showsPrec n ident
  showsPrec n (PatientDetails initials dob other) =
    showString "PatientDetails " .
    showsPrec n initials .
    showString " " .
    showsPrec n dob .
    showString " " .
    showsPrec n other
