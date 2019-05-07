{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Vessel where

import Control.Monad (join)

import Data.Semigroup (First(..))

import Data.Proxy (Proxy(..))

import Data.Functor.Classes
import Data.Functor.Identity

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Time.Calendar (Day, fromGregorian)

import Data.Dependent.Sum (EqTag(..), OrdTag(..), ShowTag(..))
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import qualified Data.Dependent.Map.Monoidal as MonoidalDMap

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Vessel

import Data.GADT.Compare.TH
import Data.GADT.Show.TH

import Data.Constraint.Extras
import Data.Constraint.Extras.TH

import Data.Validation

import Reflex.Query.Class

import Details.Common
import Disease.Version2

data DetailsKey a where
  DKInitials :: DetailsKey (IdentityV Text)
  DKDOB :: DetailsKey (IdentityV Day)
  DKWeight :: DetailsKey (SingleV Weight)
  DKDisease :: DetailsKey (DMapV ExistingConditionKey Identity)

deriveGEq ''DetailsKey
deriveGCompare ''DetailsKey
deriveGShow ''DetailsKey
deriveArgDict ''DetailsKey

deriving instance Show (DetailsKey a)

newtype Details f =
  Details {
    getDetails :: Vessel DetailsKey f
  }

deriving instance (Has' Eq DetailsKey (FlipAp f)) => Eq (Details f)

instance (Has' Ord DetailsKey (FlipAp f)) => Ord (Details f) where
  compare (Details (Vessel v1)) (Details (Vessel v2)) =
    compare v1 v2

instance (Has' Show DetailsKey (FlipAp f)) => Show (Details f) where
  showsPrec n (Details (Vessel v)) =
    showString "Details { Vessel {" .
    showsPrec n v .
    showString "}}"

instance (Has' Semigroup DetailsKey (FlipAp f)) => Semigroup (Details f) where
  Details v1 <> Details v2 = Details (v1 <> v2)

instance (Has' Monoid DetailsKey (FlipAp f)) => Monoid (Details f) where
  mempty = Details mempty
  mappend = (<>)

newDetails :: Details Maybe
newDetails =
  Details .
  fromListV $ [
    DKInitials :~> IdentityV Nothing
  , DKDOB :~> IdentityV Nothing
  , DKWeight :~> SingleV Nothing
  , DKDisease :~> DMapV MonoidalDMap.empty
  ]

checkFilled :: Details Maybe -> Maybe (Details Identity)
checkFilled = fmap Details . traverseV (fmap Identity) . getDetails

editFilled :: Details Identity -> Details Maybe
editFilled = Details . mapV (Just . runIdentity) . getDetails

-- we can condense this, would be nice to query it with the example query
exampleDetails :: Map Id (Details Identity)
exampleDetails =
  Map.fromList [ (Id 1, Details . fromListV $ [ DKInitials :~> IdentityV (Identity "ab")
                                              , DKDOB :~> IdentityV (Identity (fromGregorian 1980 1 1))
                                              ])
               , (Id 2, Details . fromListV $ [ DKInitials :~> IdentityV (Identity "cd")
                                              , DKDOB :~> IdentityV (Identity (fromGregorian 1980 1 2))
                                              ])
               , (Id 3, Details . fromListV $ [ DKInitials :~> IdentityV (Identity "ef") ])
               , (Id 4, Details . fromListV $ [ DKDOB :~> IdentityV (Identity (fromGregorian 1980 1 3)) ])
               , (Id 5, Details . fromListV $ [] )
               ]

exampleQuery :: Details Proxy
exampleQuery = Details . fromListV $ [ DKInitials :~> IdentityV Proxy, DKDOB :~> IdentityV Proxy ]

exampleResultAny :: Map Id (Details Identity)
exampleResultAny =
  fmap Details .
  disperseV .
  cropV (\_ x -> x) (getDetails exampleQuery) .
  condenseV .
  fmap getDetails $
  exampleDetails

-- not this, this is broken
exampleResultAll :: Map Id (Details Identity)
exampleResultAll =
  fmap Details .
  disperseV .
  cropV (\_ x -> x) (condenseV $ getDetails exampleQuery <$ exampleDetails) .
  condenseV .
  fmap getDetails $
  exampleDetails

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

detailsValidator :: Details Validator
detailsValidator =
  Details .
  fromListV $ [
    DKInitials :~> IdentityV (Validator validateInitials)
  , DKDOB :~> IdentityV (Validator validateDOB)
  , DKWeight :~> SingleV (Validator $ fmap (fmap First) . validateWeight . fmap getFirst)
  , DKDisease :~> DMapV _ -- MonoidalDMap.empty
  ]

validateDetails :: Details Maybe -> Validation [DetailsError] (Details Identity)
validateDetails =
  fmap Details .
  traverseV (fmap Identity) .
  cropV (\v m -> runIdentity <$> runValidator v m) (getDetails detailsValidator) .
  getDetails


