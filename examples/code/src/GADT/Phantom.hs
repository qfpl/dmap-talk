{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module GADT.Phantom where

import Data.Text (Text)
import Data.Time.Calendar (Day)

data PatientStatus = NewPatient | InSystem
  deriving (Eq, Ord, Show)

data PatientInformation (a :: PatientStatus) =
    PatientDetails Text Day -- initials and dob
  | PatientId Int           -- id in the system
  deriving (Eq, Ord, Show)

newPatient :: Text -> Day -> PatientInformation 'NewPatient
newPatient = PatientDetails

addPatientToSystem :: PatientInformation 'NewPatient -> IO (PatientInformation 'InSystem)
addPatientToSystem (PatientDetails initials dob) = pure (PatientId 0)
addPatientToSystem (PatientId i) = error "awkward..."
