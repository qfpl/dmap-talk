{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module DSum.Example where

import Data.Functor.Identity

import Data.Text (Text)
import Data.Time.Calendar (Day)

import Data.Dependent.Sum (DSum(..))
import qualified Data.Dependent.Sum as DSum

import Data.GADT.Compare
import Data.GADT.Compare.TH
import Data.GADT.Show
import Data.GADT.Show.TH

data PatientInformationTag a where
  DetailsTag :: PatientInformationTag (Text, Day)
  IdTag      :: PatientInformationTag Int

instance GEq PatientInformationTag where
  geq DetailsTag DetailsTag = Just Refl
  geq IdTag IdTag = Just Refl
  geq _ _ = Nothing

instance GCompare PatientInformationTag where
  gcompare DetailsTag DetailsTag = GEQ
  gcompare DetailsTag _ = GLT
  gcompare _ DetailsTag = GGT
  gcompare IdTag IdTag = GEQ

instance GShow PatientInformationTag where
  gshowsPrec _ DetailsTag =
    showString "DetailsTag"
  gshowsPrec _ IdTag =
    showString "IdTag"

type PatientInformation f = DSum PatientInformationTag f

addPatientToSystem :: DSum PatientInformationTag Maybe
                   -> IO (DSum PatientInformationTag Identity)
addPatientToSystem (DetailsTag :=> Nothing) =
  error "information not available"
addPatientToSystem (DetailsTag :=> Just (initials, dob)) =
  pure (IdTag :=> Identity 0) -- some process here...
addPatientToSystem (IdTag :=> Nothing) =
  error "information not available"
addPatientToSystem (IdTag :=> Just i) =
  pure (IdTag :=> Identity i)
