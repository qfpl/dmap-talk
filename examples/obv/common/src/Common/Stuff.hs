{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Common.Stuff where

import Data.Functor.Identity
import Data.Functor.Classes
import Data.Bifunctor

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

newtype Wrap a f = Wrap { unWrap :: f a }
  deriving (Eq, Ord, Show)

data Language =
  Haskell | Scala | Clojure
  deriving (Eq, Ord, Show, Bounded, Enum)

data Technology =
  Web | DB | REST | Mobile
  deriving (Eq, Ord, Show, Bounded, Enum)

data ManagementRole =
  TeamLead | ProductManager | ProjectManager
  deriving (Eq, Ord, Show, Bounded, Enum)

-- this can also be interesting
-- possibly moreso when we add vessel into the mix
-- data ManagementRole f =
--   ManagementRole {
--     mrType :: f ManagementType
--   , mrYears :: f Years
--   } deriving (Eq, Ord, Show)

data Methodology =
  Scrum | Agile | Waterfall
  deriving (Eq, Ord, Show, Bounded, Enum)

newtype Years = Years { getYears :: Int }
  deriving (Eq, Ord, Show, Read)

data ProgrammerKey a where
  PKLanguage :: Language -> ProgrammerKey Years
  PKTechnology :: Technology -> ProgrammerKey Bool

deriveGEq ''ProgrammerKey
deriveGCompare ''ProgrammerKey
deriveGShow ''ProgrammerKey
deriveArgDict ''ProgrammerKey

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
  deriving (Eq, Ord, Show )

-- multiple methodologies of interest, Methodology is an argument to the key
-- if we're talking about the primary role, we might have MKRole :: ManagerKey ManagementRole
-- ManagementRole might have ManagementType and Years, which would be interesting for a Years prism
data ManagerKey a where
  MKRole :: ManagementRole -> ManagerKey Years
  MKMethodology :: Methodology -> ManagerKey Bool

deriveGEq ''ManagerKey
deriveGCompare ''ManagerKey
deriveGShow ''ManagerKey
deriveArgDict ''ManagerKey

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
  deriving (Eq, Ord, Show)

data AttendeeKey a where
  -- can avoid the need for constraints here if we use some of the things in constraints-extras
  -- AKManager :: (Eq a, Ord a, Show a) => ManagerKey a -> AttendeeKey a
  -- AKProgrammer :: (Eq a, Ord a, Show a) => ProgrammerKey a -> AttendeeKey a
  AKManager :: ManagerKey a -> AttendeeKey a
  AKProgrammer :: ProgrammerKey a -> AttendeeKey a

deriveGEq ''AttendeeKey
deriveGCompare ''AttendeeKey
deriveArgDict ''AttendeeKey

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
  deriving (Eq, Ord, Show)

instance Semigroup (Attendee f) where
  Attendee dm1 <> Attendee dm2 = Attendee (dm1 <> dm2)

instance Monoid (Attendee f) where
  mempty = Attendee mempty
  mappend = (<>)

initialAttendee :: Attendee Maybe
initialAttendee = Attendee $
    DMap.fromList [
      AKProgrammer (PKLanguage Haskell) :=> Nothing
    , AKProgrammer (PKLanguage Scala) :=> Nothing
    , AKProgrammer (PKLanguage Clojure) :=> Nothing
    , AKProgrammer (PKTechnology Web) :=> Nothing
    , AKProgrammer (PKTechnology DB) :=> Nothing
    , AKProgrammer (PKTechnology REST) :=> Nothing
    , AKProgrammer (PKTechnology Mobile) :=> Nothing
    , AKManager (MKRole TeamLead) :=> Nothing
    , AKManager (MKRole ProductManager) :=> Nothing
    , AKManager (MKRole ProjectManager) :=> Nothing
    , AKManager (MKMethodology Scrum) :=> Nothing
    , AKManager (MKMethodology Agile) :=> Nothing
    , AKManager (MKMethodology Waterfall) :=> Nothing
    ]

