{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Disease.Version2 where

import Data.Functor.Identity

import Data.Dependent.Sum (DSum(..))
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap

import Data.GADT.Compare.TH
import Data.GADT.Show.TH

import Disease.Common

data ExistingConditionKey a where
  ECDiabetes :: ExistingConditionKey DiabetesInfo
  ECCancer :: CancerType -> ExistingConditionKey CancerStage
  ECEpilepsy :: ExistingConditionKey ()

deriveGEq ''ExistingConditionKey
deriveGCompare ''ExistingConditionKey
deriveGShow ''ExistingConditionKey
-- deriveEqTagIdentity ''ExistingConditionKey
-- deriveOrdTagIdentity ''ExistingConditionKey
-- deriveShowTagIdentity ''ExistingConditionKey

newtype ExistingConditions f =
  ExistingConditions {
  -- perhaps dependent-monoidal-map might be better?
    unExistingConditions :: DMap ExistingConditionKey f
  -- } deriving (Eq, Ord, Show, Semigroup, Monoid)
    } deriving (Semigroup, Monoid)


