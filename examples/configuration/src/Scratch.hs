{-# LANGUAGE GADTs #-}
module Scratch where

import Data.Dependent.Sum (DSum(..))
import qualified Data.Dependent.Sum as DSum

import Data.Dependent.Map (DMap(..))
import qualified Data.Dependent.Map as DMap

import Data.GADT.Compare.TH
import Data.GADT.Show.TH

data Shape = Square | Circle
  deriving (Eq, Ord, Show)

data OnMissing a =
    OMSkip
  | OMDefault a
  deriving (Eq, Ord, Show)

class Continuous a where
  asDouble :: a -> Double

-- hmm - do we want a collection of (classy?) lenses / prisms here,
-- in order to guarantee the presence of keys in the data...

data Geom k where
  Line :: (Continuous x, Continuous y) => k x -> k y -> Geom k
  Point :: (Continuous x, Continuous y) => k x -> k y -> k Shape -> Geom k

-- data source + mapping
-- mapping and geom interact
-- - geom has requirements from the mapping
--   - maybe just points and lines? that way we can stick with continuous data
-- axis needs info from the mapping

-- lines are x / y
-- - x / y are required
-- points are x / y / shape / colour / size
-- - x / y are required
-- - shape / colour / size get defaulted in if missing
--   - are part of the "do we delete this data point for missing data" consideration
--   - so if they do get mapped across, the mapping needs to happen
