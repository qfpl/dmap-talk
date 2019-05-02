{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Details.Version4 where

import Data.Functor.Classes

import Data.Text (Text)
import Data.Time.Calendar (Day)

import Data.Dependent.Sum (EqTag(..), OrdTag(..), ShowTag(..))
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap

import Data.Vessel

import Data.GADT.Compare.TH
import Data.GADT.Show.TH

import Data.Constraint.Extras
import Data.Constraint.Extras.TH

import Details.Common

data DetailsKey a where
  DKId :: DetailsKey (IdentityV Id)
  DKInitials :: DetailsKey (IdentityV Id)
  DKDOB :: DetailsKey (IdentityV Id)
  DKWeight :: DetailsKey (SingleV Weight)

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
    DKId :~> IdentityV Nothing
  , DKInitials :~> IdentityV Nothing
  , DKDOB :~> IdentityV Nothing
  , DKWeight :~> SingleV Nothing
  ]
