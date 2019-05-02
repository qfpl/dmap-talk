{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Details.Version3 where

import Control.Monad (join)

import Data.Proxy (Proxy(..))
import Data.Functor.Identity
-- import Data.Functor.Classes

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)

import Data.Dependent.Sum(DSum(..), EqTag(..), OrdTag(..), ShowTag(..))
import Data.Dependent.Map.Monoidal (MonoidalDMap)
import qualified Data.Dependent.Map.Monoidal as MonoidalDMap

import Data.GADT.Compare.TH
import Data.GADT.Show.TH

import Data.Constraint.Extras
import Data.Constraint.Extras.TH

import Data.Validation

import Details.Common

data DetailsKey a where
  DKId :: DetailsKey Id
  DKInitials :: DetailsKey Text
  DKDOB :: DetailsKey Day
  DKWeight :: DetailsKey (Maybe Weight)

deriveGEq ''DetailsKey
deriveGCompare ''DetailsKey
deriveGShow ''DetailsKey
deriveArgDict ''DetailsKey

-- required for Show of Details
deriving instance Show (DetailsKey a)

-- instance Eq1 f => EqTag DetailsKey f where
--   eqTagged DKId DKId = eq1
--   eqTagged DKInitials DKInitials = eq1
--   eqTagged DKDOB DKDOB = eq1
--   eqTagged DKWeight DKWeight = eq1

-- instance Ord1 f => OrdTag DetailsKey f where
--   compareTagged DKId DKId = compare1
--   compareTagged DKInitials DKInitials = compare1
--   compareTagged DKDOB DKDOB = compare1
--   compareTagged DKWeight DKWeight = compare1

-- instance Show1 f => ShowTag DetailsKey f where
--   showTaggedPrec DKId n = showsPrec1 n
--   showTaggedPrec DKInitials n = showsPrec1 n
--   showTaggedPrec DKDOB n = showsPrec1 n
--   showTaggedPrec DKWeight n = showsPrec1 n

newtype Details f =
  Details {
    getDetails :: MonoidalDMap DetailsKey f
  }

-- deriving instance Eq1 f => Eq (Details f)
-- deriving instance Ord1 f => Ord (Details f)
-- deriving instance Show1 f => Show (Details f)
deriving instance (Has' Eq DetailsKey f) => Eq (Details f)
deriving instance (Has' Ord DetailsKey f) => Ord (Details f)
deriving instance (Has' Show DetailsKey f) => Show (Details f)
deriving instance (Has' Semigroup DetailsKey f) => Semigroup (Details f)
deriving instance (Has' Monoid DetailsKey f) => Monoid (Details f)

newDetails :: Details Maybe
newDetails =
  Details .
  MonoidalDMap.fromAscListWithKey (\_ v _ -> v) $
  [ DKId :=> Nothing
  , DKInitials :=> Nothing
  , DKDOB :=> Nothing
  , DKWeight :=> Nothing
  ]

checkFilled :: Details Maybe -> Maybe (Details Identity)
checkFilled =
  fmap Details .
  MonoidalDMap.traverseWithKey (\_ -> fmap Identity) .
  getDetails

editFilled :: Details Identity -> Details Maybe
editFilled =
  Details .
  MonoidalDMap.mapWithKey (\_ -> Just . runIdentity) .
  getDetails

data DetailsError = NotSet | InitialsTooShort
  deriving (Eq, Ord, Show)

newtype Validator x = Validator { runValidator :: Maybe x -> Validation [DetailsError] (Identity x) }

validateIdent :: Maybe Id -> Validation [DetailsError] (Identity Id)
validateIdent Nothing = Failure [NotSet]
validateIdent (Just i) = Success (Identity i)

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

detailsValidator :: Details Validator
detailsValidator =
  Details .
  MonoidalDMap.fromListWithKey (\_ v _ -> v) $
  [ DKId       :=> Validator validateIdent
  , DKInitials :=> Validator validateInitials
  , DKDOB      :=> Validator validateDOB
  , DKWeight   :=> Validator validateWeight
  ]

validateDetails :: Details Maybe -> Validation [DetailsError] (Details Identity)
validateDetails =
  fmap Details .
  MonoidalDMap.traverseWithKey (\_ v -> fmap Identity v) .
  MonoidalDMap.intersectionWithKey (\_ vv vm -> runIdentity <$> runValidator vv vm) (getDetails detailsValidator) .
  getDetails

-- add querying with proxy, or what until we have View so we can do things with maps?

-- queryDetails :: Details Proxy -> Details Identity -> Details Maybe

