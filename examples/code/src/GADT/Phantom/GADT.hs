{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module GADT.Phantom.GADT where

import Data.Text (Text)
import Data.Time.Calendar (Day)

data PatientStatus = NewPatient | InSystem
  deriving (Eq, Ord, Show)

data PatientInformation (a :: PatientStatus) where
  PatientDetails :: Text -> Day -> PatientInformation 'NewPatient -- initials and dob
  PatientId :: Int -> PatientInformation 'InSystem                -- id in the system

addPatientToSystem :: PatientInformation 'NewPatient -> IO (PatientInformation 'InSystem)
addPatientToSystem (PatientDetails initials dob) = pure (PatientId 0)
