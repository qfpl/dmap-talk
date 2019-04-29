{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Diagrams.GG.Types where

import Data.Maybe (fromMaybe)
import Data.Semigroup (Option(..), Last(..))

import Data.Text (Text)

import Data.Constraint.Extras

import Data.Dependent.Sum
import Data.Dependent.Map (DMap, Some(..))
import qualified Data.Dependent.Map as DMap

import Data.GADT.Compare (GEq(..), GCompare(..), (:~:)(..), GOrdering(..))
import Data.GADT.Show (GShow(..))

data Shape =
    Square
  | Circle
  deriving (Eq, Ord, Show)

class Num a => Continuous a where
  asDouble :: a -> Double

instance Continuous Double where
  asDouble = id

instance Continuous Int where
  asDouble = fromIntegral

data OnMissing a =
    OMSkip
  | OMDefault a
  deriving (Eq, Ord, Show)

data ColumnMetadata a =
  ColumnMetadata {
    _cmName :: Text
  , _cmOnMissing :: OnMissing a
  } deriving (Eq, Ord, Show)

data Column f a =
  Column {
    _cMetadata :: ColumnMetadata a
  , _cData :: f a
  } deriving (Eq, Ord, Show)

data ColumnStats a =
  ColumnStats {
    _csMin :: Maybe a
  , _csMax :: Maybe a
  , _csCount :: Int
  } deriving (Eq, Ord, Show)

instance Functor ColumnStats where
  fmap f (ColumnStats mi ma c) =
    ColumnStats (fmap f mi) (fmap f ma) c

csRange :: Num a => ColumnStats a -> Maybe a
csRange cs =
  (-) <$> _csMax cs <*> _csMin cs

gatherColumnStats :: (Foldable f, Ord a)
                  => f a
                  -> ColumnStats a
gatherColumnStats =
  L.fold (ColumnStats <$> L.minimum <*> L.maximum <*> L.length)

gatherFrameState :: (Foldable f, Has Ord k)
                 => DMap k (Column f)
                 -> DMap k ColumnStats
gatherFrameState =
  DMap.mapWithKey (\k v -> has @Ord k (gatherColumnStats . _cData $ v))

data Mapping k where
  -- we are assuming continuous x and y for the time being
  Mapping :: (Continuous x, Continuous y)
          => k x
          -> k y
          -> Mapping k

instance GEq k => Eq (Mapping k) where
  Mapping x1 y1 == Mapping x2 y2 =
    fromMaybe False $ do
      Refl <- geq x1 x2
      Refl <- geq y1 y2
      pure True

instance GCompare k => Ord (Mapping k) where
  compare (Mapping x1 y1) (Mapping x2 y2) =
    case gcompare x1 x2 of
      GEQ -> case gcompare y1 y2 of
        GEQ -> EQ
        GLT -> LT
        GGT -> GT
      GLT -> LT
      GGT -> GT

instance GShow k => Show (Mapping k) where
  showsPrec n (Mapping x y) =
    showString "Mapping " . gshowsPrec n x . showString " " . gshowsPrec n y

data Layer k f =
  Layer {
    _lData :: DMap k (Column f)
  , _lMapping :: Mapping k
  }

instance (GEq k, Has Eq k, Has' Eq k f) => EqTag k (Column f) where
  eqTagged k _ c1 c2 =
    has @Eq k (has' @Eq @f k (c1 == c2))

instance (GCompare k, Has Eq k, Has Ord k, Has' Eq k f, Has' Ord k f) => OrdTag k (Column f) where
  compareTagged k _ c1 c2 =
    has @Ord k (has' @Ord @f k (compare c1 c2))

instance (GShow k, Has Show k, Has' Show k f) => ShowTag k (Column f) where
  showTaggedPrec k n c =
    has @Show k (has' @Show @f k (showsPrec n c))

deriving instance (GEq k, Has Eq k, Has' Eq k f) => Eq (Layer k f)
deriving instance (GCompare k, Has Eq k, Has Ord k, Has' Eq k f, Has' Ord k f) => Ord (Layer k f)
deriving instance (GShow k, Has Show k, Has' Show k f) => Show (Layer k f)

data GeomType =
    Point
  | Line
  | Bar
  deriving (Eq, Ord, Show)

-- we want attributes for each geom, and the kinds of attributes are going to be different
-- they are also going to have different needs with respect to data
-- point has position, colour, shape, size
-- line has position, colour?, size?
-- bar has position, height, width and fill colour

data ScaleType =
    Continuous
  | Log10
  deriving (Eq, Ord, Show)

data Ticks =
  Ticks {
    _tMajor :: [Double]
  , _tMinor :: [Double]
  } deriving (Eq, Ord, Show)

data Scale =
  Scale {
    _sType :: ScaleType
  , _sFn :: ColumnStats Double -> Ticks
  }

defaultContinuousScaler :: ColumnStats Double -> Ticks
defaultContinuousScaler (ColumnStats (Just lo) (Just hi) c) =
  defaultContinuousScaler' lo hi c
defaultContinuousScaler _ =
  defaultContinuousScaler' 0 10 0

addMinorTicks :: Double -> Double -> Double -> [Double] -> Ticks
addMinorTicks lo hi majorStep [] =
  Ticks [] $ []
addMinorTicks lo hi majorStep majors@(m : ms) =
  -- minor ticks are halfway between major ticks and also before / after major ticks if that still fits within the range
  let
    minorStep = majorStep / 2.0
    minors = filter (\x -> lo <= x && x <= hi) $ (m - minorStep) : fmap (+ minorStep) majors
  in
    Ticks majors minors

defaultContinuousScaler' :: Double -> Double -> Int -> Ticks
defaultContinuousScaler' lo hi c =
  let
  -- major ticks:
  -- we want between 3 and 5 major ticks worth
  -- single steps through powers of 10 would be ideal
  -- multiples of 5 if that won't work
    majorStep = 0.0
    majors = []
  in
    addMinorTicks lo hi majorStep majors

continuous :: Scale
continuous = Scale Continuous defaultContinuousScaler

data CoordinateSystem =
    Cartesian
  -- | Polar
  deriving (Eq, Ord, Show)

-- TODO Wrap all of these in Last,
-- use that for the Semigroup and Monoid instances,
-- have a default value that gets used behind the scenes to supply defaults
data Graph =
  Graph {
    _gLayers :: [Int] -- this is the data output from whatever we do with layers
  , _gXScale :: Option (Last Scale)
  , _gYScale :: Option (Last Scale)
  , _gCoord :: Option (Last CoordinateSystem)
  }

defaultGraph :: Graph
defaultGraph =
  Graph
    []
    (pure continuous)
    (pure continuous)
    (pure Cartesian)

-- scale has
--   - how to select the major and minor ticks from the data range

-- we need theme options, and they are potentially going to be split all over the place
-- - scales
--   - colours, sizes and fonts of major and minor ticks
--   - offset, colour, sizes and fonts for the axis label
-- - background grid (cartesian)
--   - colours of empty space, colours and sizes of major and minor ticks
