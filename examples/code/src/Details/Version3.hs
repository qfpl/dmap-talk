{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Details.Version3 where

import Data.Text (Text)
import Data.Time.Calendar (Day)

import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap

import Data.GADT.Compare.TH
import Data.GADT.Show.TH

import Details.Common

data DetailsKey a where
  DKId :: DetailsKey Id
  DKInitials :: DetailsKey Text
  DKDOB :: DetailsKey Day
  DKWeight :: DetailsKey (Maybe Weight)

deriveGEq ''DetailsKey
deriveGCompare ''DetailsKey
deriveGShow ''DetailsKey

newtype Details f =
  Details {
    getDetails :: DMap DetailsKey f
  -- } deriving (Eq, Ord, Show)
  }