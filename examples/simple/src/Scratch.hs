{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-} -- for deriveArgDict ''AttendeeKey
module Scratch where

import GHC.Generics (Generic)

import Data.Functor.Identity
import Data.Functor.Classes

import Data.Dependent.Sum (DSum(..), EqTag(..), OrdTag(..), ShowTag(..))
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Some (Some)
import qualified Data.Some as Some

import Data.Constraint.Forall
import Data.Constraint.Extras
import Data.Constraint.Extras.TH

import Data.GADT.Compare
import Data.GADT.Compare.TH

import Data.GADT.Show
import Data.GADT.Show.TH

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.GADT.TH
-- import Data.Dependent.Sum.Orphans

import Control.Lens hiding (has)

instance (ToJSON1 f, ForallF ToJSON k, Has ToJSON k) => ToJSON (DSum k f) where
  toJSON ((k :: k a) :=> (f :: f a))
    = whichever @ToJSON @k @a $
        has @ToJSON k $
          let x = toJSON k
              y = toJSON1 f
          in
            toJSON (x, y)

instance (ToJSON1 f, ForallF ToJSON k, Has ToJSON k) => ToJSON (DMap k f) where
  toJSON = toJSON . DMap.toList

instance (FromJSON1 f, FromJSON (Some k), Has FromJSON k) => FromJSON (DSum k f) where
  parseJSON x = do
    (jk, jf) <- parseJSON x
    Some.This (k :: k a) <- parseJSON jk
    f <- has @FromJSON k $ parseJSON1 jf
    return $ k :=> f

instance (FromJSON1 f, FromJSON (Some k), GCompare k, Has FromJSON k) => FromJSON (DMap k f) where
  parseJSON = fmap DMap.fromList . parseJSON

data Language =
  Haskell | Scala | Clojure | Erlang | Elixir
  deriving (Eq, Ord, Show)

deriveJSON defaultOptions ''Language

data Technology =
  Web | DB | REST | Mobile
  deriving (Eq, Ord, Show)

deriveJSON defaultOptions ''Technology

data ManagementRole =
  TeamLead | ProductManager | ProjectManager
  deriving (Eq, Ord, Show)

deriveJSON defaultOptions ''ManagementRole

-- this can also be interesting
-- possibly moreso when we add vessel into the mix
-- data ManagementRole f =
--   ManagementRole {
--     mrType :: f ManagementType
--   , mrYears :: f Years
--   } deriving (Eq, Ord, Show)

data Methodology =
  Scrum | Agile | Waterfall
  deriving (Eq, Ord, Show)

deriveJSON defaultOptions ''Methodology

newtype Years = Years { getYears :: Int }
  deriving (Eq, Ord, Show)

deriveJSON defaultOptions ''Years

data ProgrammerKey a where
  PKLanguage :: Language -> ProgrammerKey Years
  PKTechnology :: Technology -> ProgrammerKey ()

deriveGEq ''ProgrammerKey
deriveGCompare ''ProgrammerKey
deriveGShow ''ProgrammerKey
deriveArgDict ''ProgrammerKey
deriveJSONGADT ''ProgrammerKey

instance Eq1 f => EqTag ProgrammerKey f where
  eqTagged (PKLanguage l1) (PKLanguage l2) f1 f2
    | l1 == l2 = eq1 f1 f2
    | otherwise = False
  eqTagged (PKTechnology t1) (PKTechnology t2) f1 f2
    | t1 == t2 = eq1 f1 f2
    | otherwise = False

instance Ord1 f => OrdTag ProgrammerKey f where
  compareTagged (PKLanguage l1) (PKLanguage l2) f1 f2 =
    case compare l1 l2 of
      LT -> LT
      GT -> GT
      EQ -> compare1 f1 f2
  compareTagged (PKTechnology t1) (PKTechnology t2) f1 f2 =
    case compare t1 t2 of
      LT -> LT
      GT -> GT
      EQ -> compare1 f1 f2

instance Show1 f => ShowTag ProgrammerKey f where
  showTaggedPrec (PKLanguage _) n = showsPrec1 n
  showTaggedPrec (PKTechnology _) n = showsPrec1 n

newtype Programmer f = Programmer { unProgrammer :: DMap ProgrammerKey f }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON1 f => ToJSON (Programmer f)
instance FromJSON1 f => FromJSON (Programmer f)

-- multiple methodologies of interest, Methodology is an argument to the key
-- if we're talking about the primary role, we might have MKRole :: ManagerKey ManagementRole
-- ManagementRole might have ManagementType and Years, which would be interesting for a Years prism
data ManagerKey a where
  MKRole :: ManagementRole -> ManagerKey Years
  MKMethodology :: Methodology -> ManagerKey ()

deriveGEq ''ManagerKey
deriveGCompare ''ManagerKey
deriveGShow ''ManagerKey
deriveArgDict ''ManagerKey
deriveJSONGADT ''ManagerKey

instance Eq1 f => EqTag ManagerKey f where
  eqTagged (MKRole r1) (MKRole r2) f1 f2
    | r1 == r2 = eq1 f1 f2
    | otherwise = False
  eqTagged (MKMethodology m1) (MKMethodology m2) f1 f2
    | m1 == m2 = eq1 f1 f2
    | otherwise = False

instance Ord1 f => OrdTag ManagerKey f where
  compareTagged (MKRole r1) (MKRole r2) f1 f2 =
    case compare r1 r2 of
      LT -> LT
      GT -> GT
      EQ -> compare1 f1 f2
  compareTagged (MKMethodology m1) (MKMethodology m2) f1 f2 =
    case compare m1 m2 of
      LT -> LT
      GT -> GT
      EQ -> compare1 f1 f2

instance Show1 f => ShowTag ManagerKey f where
  showTaggedPrec (MKRole _) n = showsPrec1 n
  showTaggedPrec (MKMethodology _) n = showsPrec1 n

newtype Manager f = Manager { unManager :: DMap ManagerKey f }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON1 f => ToJSON (Manager f)
instance FromJSON1 f => FromJSON (Manager f)

data AttendeeKey a where
  -- can avoid the need for constraints here if we use some of the things in constraints-extras
  -- AKManager :: (Eq a, Ord a, Show a) => ManagerKey a -> AttendeeKey a
  -- AKProgrammer :: (Eq a, Ord a, Show a) => ProgrammerKey a -> AttendeeKey a
  AKManager :: ManagerKey a -> AttendeeKey a
  AKProgrammer :: ProgrammerKey a -> AttendeeKey a

deriveGEq ''AttendeeKey
deriveGCompare ''AttendeeKey
deriveArgDict ''AttendeeKey
deriveJSONGADT ''AttendeeKey

instance GShow AttendeeKey where
  gshowsPrec n (AKManager k) = gshowsPrec n k
  gshowsPrec n (AKProgrammer k) = gshowsPrec n k

instance Eq1 f => EqTag AttendeeKey f where
  eqTagged (AKManager k1) (AKManager k2) f1 f2 =
    case geq k1 k2 of
      Just Refl -> has @Eq k1 $ eq1 f1 f2
      _ -> False
  eqTagged (AKProgrammer k1) (AKProgrammer k2) f1 f2 =
    case geq k1 k2 of
      Just Refl -> has @Eq k1 $ eq1 f1 f2
      _ -> False
  eqTagged _ _ _ _ =
    False

instance Ord1 f => OrdTag AttendeeKey f where
  compareTagged (AKManager k1) (AKManager k2) f1 f2 =
    case gcompare k1 k2 of
      GLT -> LT
      GGT -> GT
      GEQ -> has @Ord k1 $ compare1 f1 f2
  compareTagged (AKManager _) _ _ _ =
    LT
  compareTagged _ (AKManager _) _ _ =
    GT
  compareTagged (AKProgrammer k1) (AKProgrammer k2) f1 f2 =
    case gcompare k1 k2 of
      GLT -> LT
      GGT -> GT
      GEQ -> has @Ord k1 $ compare1 f1 f2

instance Show1 f => ShowTag AttendeeKey f where
  showTaggedPrec (AKManager k) n f = has @Show k $ showsPrec1 n f
  showTaggedPrec (AKProgrammer k) n f = has @Show  k $ showsPrec1 n f

newtype Attendee f = Attendee { unAttendee :: DMap AttendeeKey f }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON1 f => ToJSON (Attendee f)
instance FromJSON1 f => FromJSON (Attendee f)

testAttendeeMaybe :: Attendee Maybe
testAttendeeMaybe =
  Attendee .
  DMap.fromList $
    [ AKProgrammer (PKLanguage Haskell) :=> Just (Years 5)
    , AKManager (MKMethodology Agile) :=> Just ()
    ]

allRequired :: Attendee Maybe -> Maybe (Attendee Identity)
allRequired (Attendee dm) =
  Attendee <$> DMap.traverseWithKey (\_ v -> fmap Identity v) dm

anyRequired :: Attendee Maybe -> Attendee Identity
anyRequired (Attendee dm) =
  Attendee $ DMap.mapMaybeWithKey (\_ v -> fmap Identity v) dm

-- class HasYears k where
--   _Years :: Prism' (k Years) ()

-- instance HasYears ProgrammerKey where
--   _Years = prism _ _