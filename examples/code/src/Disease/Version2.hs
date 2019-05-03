{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Disease.Version2 where

import Data.Functor.Classes
import Data.Functor.Identity

import Data.Dependent.Sum (DSum(..), EqTag(..), OrdTag(..), ShowTag(..))
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap

import Data.GADT.Compare.TH
import Data.GADT.Show.TH

import Data.Constraint.Extras.TH

import Disease.Common

data ExistingConditionKey a where
  ECDiabetes :: ExistingConditionKey DiabetesInfo
  ECCancer :: CancerType -> ExistingConditionKey CancerStage
  ECEpilepsy :: ExistingConditionKey ()

deriveGEq ''ExistingConditionKey
deriveGCompare ''ExistingConditionKey
deriveGShow ''ExistingConditionKey
deriveArgDict ''ExistingConditionKey

deriving instance Show (ExistingConditionKey a)

instance Eq1 f => EqTag ExistingConditionKey f where
  eqTagged ECDiabetes ECDiabetes =
    eq1
  eqTagged (ECCancer c1) (ECCancer c2)
    | c1 == c2 = eq1
    | otherwise = \_ _ -> False
  eqTagged ECEpilepsy ECEpilepsy =
    eq1

instance Ord1 f => OrdTag ExistingConditionKey f where
  compareTagged ECDiabetes ECDiabetes =
    compare1
  compareTagged (ECCancer c1) (ECCancer c2) =
    case compare c1 c2 of
      LT -> \_ _ -> LT
      GT -> \_ _ -> GT
      EQ -> compare1
  compareTagged ECEpilepsy ECEpilepsy =
    compare1

instance Show1 f => ShowTag ExistingConditionKey f where
  showTaggedPrec ECDiabetes = showsPrec1
  showTaggedPrec (ECCancer _) = showsPrec1
  showTaggedPrec ECEpilepsy = showsPrec1

newtype ExistingConditions f =
  ExistingConditions {
  -- perhaps dependent-monoidal-map might be better?
    unExistingConditions :: DMap ExistingConditionKey f
  } deriving (Eq, Ord, Show, Semigroup, Monoid)


