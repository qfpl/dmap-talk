{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module DMap where

import Control.Monad (join)

import Data.Functor.Classes
import Data.Functor.Identity

import Data.Maybe (fromMaybe)

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Time.Calendar (Day, fromGregorian)

import Data.Dependent.Sum (DSum(..), EqTag(..), OrdTag(..), ShowTag(..))
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap

import Data.GADT.Compare
import Data.GADT.Compare.TH
import Data.GADT.Show
import Data.GADT.Show.TH

import Data.Constraint.Extras.TH

import Data.Validation

import Details.Common
import Disease.Version2

data DetailsKey a where
  DKInitials :: DetailsKey Text
  DKDOB      :: DetailsKey Day
  DKWeight   :: DetailsKey (Maybe Weight)
  DKDisease  :: DetailsKey (DMap ExistingConditionKey Identity)

deriveGEq ''DetailsKey
deriveGCompare ''DetailsKey
deriveGShow ''DetailsKey
deriveArgDict ''DetailsKey

deriving instance Show (DetailsKey a)

instance Eq1 f => EqTag DetailsKey f where
  eqTagged DKInitials DKInitials = eq1
  eqTagged DKDOB DKDOB = eq1
  eqTagged DKWeight DKWeight = eq1
  eqTagged DKDisease DKDisease = eq1

instance Ord1 f => OrdTag DetailsKey f where
  compareTagged DKInitials DKInitials = compare1
  compareTagged DKDOB DKDOB = compare1
  compareTagged DKWeight DKWeight = compare1
  compareTagged DKDisease DKDisease = compare1

instance Show1 f => ShowTag DetailsKey f where
  showTaggedPrec DKInitials _ _ = showString "DKInitials"
  showTaggedPrec DKDOB _ _ = showString "DKDOB"
  showTaggedPrec DKWeight _ _ = showString "DKWeight"
  showTaggedPrec DKDisease _ _ = showString "DKDisease"

newtype Details f =
  Details {
    getDetails :: DMap DetailsKey f
  }

deriving instance Eq1 f => Eq (Details f)
deriving instance Ord1 f => Ord (Details f)
deriving instance Show1 f => Show (Details f)

newDetails :: Details Maybe
newDetails =
  Details .
  DMap.fromList $ [
    DKInitials :=> Nothing
  , DKDOB :=> Nothing
  , DKWeight :=> Nothing
  , DKDisease :=> Just DMap.empty
  ]

checkFilled :: Details Maybe -> Maybe (Details Identity)
checkFilled =
  fmap Details .
  DMap.traverseWithKey (\_ -> fmap Identity) .
  getDetails

editFilled :: Details Identity -> Details Maybe
editFilled =
  Details .
  DMap.map (Just . runIdentity) .
  getDetails

data DetailsError = NotSet | InitialsTooShort
  deriving (Eq, Ord, Show)

newtype Validator x = Validator { runValidator :: Maybe x -> Validation [DetailsError] (Identity x) }

validateInitials :: Maybe Text -> Validation [DetailsError] (Identity Text)
validateInitials Nothing = Failure [NotSet]
validateInitials (Just t)
  | Text.length t < 2 = Failure [InitialsTooShort]
  | otherwise = Success (Identity t)

validateDOB :: Maybe Day -> Validation [DetailsError] (Identity Day)
validateDOB Nothing = Failure [NotSet]
validateDOB (Just d) = Success (Identity d)

validateWeight :: Maybe (Maybe Weight) -> Validation [DetailsError] (Identity (Maybe Weight))
validateWeight mmw = Success (Identity (join mmw))

validateDisease :: Maybe (DMap ExistingConditionKey Identity) -> Validation [DetailsError] (Identity (DMap ExistingConditionKey Identity))
validateDisease md = Success (Identity (fromMaybe DMap.empty md))

detailsValidator :: Details Validator
detailsValidator =
  Details .
  DMap.fromList $ [
    DKInitials :=> Validator validateInitials
  , DKDOB      :=> Validator validateDOB
  , DKWeight   :=> Validator validateWeight
  , DKDisease  :=> Validator validateDisease
  ]

validateDetails :: Details Maybe -> Validation [DetailsError] (Details Identity)
validateDetails =
  fmap Details .
  DMap.traverseWithKey (\_ -> fmap Identity) .
  DMap.intersectionWithKey (\_ v m -> runIdentity <$> runValidator v m) (getDetails detailsValidator) .
  getDetails
