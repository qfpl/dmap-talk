module Disease.Version1 where

import Data.Monoid (Any(..), Last(..))

import Data.Map (Map)
import qualified Data.Map as Map

import Disease.Common

data ExistingConditions =
  ExistingConditions {
    ecDiabetes :: Last DiabetesInfo
  , ecCancer :: Map CancerType CancerStage
  , ecEpilepsy :: Any
  } deriving (Eq, Ord, Show)

instance Semigroup ExistingConditions where
  ExistingConditions d1 c1 e1 <> ExistingConditions d2 c2 e2 =
    ExistingConditions (d1 <> d2) (c1 <> c2) (e1 <> e2)

instance Monoid ExistingConditions where
  mempty = ExistingConditions mempty mempty mempty
  mappend = (<>)