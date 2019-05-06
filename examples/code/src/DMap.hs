{-# LANGUAGE GADTs #-}
module DMap where

import Data.Functor.Identity

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Time.Calendar (Day, fromGregorian)

import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap

import Details.Common
import Disease.Version2

data DetailsKey a where
  DKInitials :: DetailsKey Text
  DKDOB      :: DetailsKey Day
  DKWeight   :: DetailsKey (Maybe Weight)
  DKDisease  :: DetailsKey (DMap ExistingConditionKey Identity)
