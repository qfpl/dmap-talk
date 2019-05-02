{-# LANGUAGE ExistentialQuantification #-}
module GADT.Existential where

import Data.Text (Text)
import Data.Time.Calendar (Day)

data PatientInformation = forall a. Show a =>
    PatientDetails Text Day a -- initials and dob and other
  | PatientId Int           -- id in the system

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

