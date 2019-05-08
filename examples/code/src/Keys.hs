{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Keys where

import Data.Functor.Classes
import Data.Proxy (Proxy(..))

import Data.Text (Text)
import Data.Time.Calendar (Day)

import Data.Dependent.Sum (DSum(..), EqTag(..), OrdTag(..), ShowTag(..))

import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap

import Data.GADT.Compare
import Data.GADT.Compare.TH
import Data.GADT.Show
import Data.GADT.Show.TH

import Control.Lens

data NewPatientKey a where
  NPKInitials :: NewPatientKey Text
  NPKDOB      :: NewPatientKey Day

deriveGEq ''NewPatientKey
deriveGCompare ''NewPatientKey
deriveGShow ''NewPatientKey

deriving instance Show (NewPatientKey a)

instance Eq1 f => EqTag NewPatientKey f where
  eqTagged NPKInitials NPKInitials = eq1
  eqTagged NPKDOB NPKDOB = eq1

instance Ord1 f => OrdTag NewPatientKey f where
  compareTagged NPKInitials NPKInitials = compare1
  compareTagged NPKDOB NPKDOB = compare1

instance Show1 f => ShowTag NewPatientKey f where
  showTaggedPrec NPKInitials = showsPrec1
  showTaggedPrec NPKDOB = showsPrec1

data InSystemKey a where
  ISKId :: InSystemKey Int

deriveGEq ''InSystemKey
deriveGCompare ''InSystemKey
deriveGShow ''InSystemKey

deriving instance Show (InSystemKey a)

instance Eq1 f => EqTag InSystemKey f where
  eqTagged ISKId ISKId = eq1

instance Ord1 f => OrdTag InSystemKey f where
  compareTagged ISKId ISKId = compare1

instance Show1 f => ShowTag InSystemKey f where
  showTaggedPrec ISKId = showsPrec1

data DetailsKey a where
  DKNewPatient :: NewPatientKey a -> DetailsKey a
  DKInSystem   :: InSystemKey a   -> DetailsKey a

deriveGEq ''DetailsKey
deriveGCompare ''DetailsKey
deriveGShow ''DetailsKey

deriving instance Show (DetailsKey a)

instance Eq1 f => EqTag DetailsKey f where
  eqTagged (DKNewPatient k1) (DKNewPatient k2) = eqTagged k1 k2
  eqTagged (DKInSystem k1) (DKInSystem k2) = eqTagged k1 k2
  eqTagged _ _ = \_ _ -> False

instance Ord1 f => OrdTag DetailsKey f where
  compareTagged (DKNewPatient k1) (DKNewPatient k2) = compareTagged k1 k2
  compareTagged (DKNewPatient _) _ = \_ _ -> LT
  compareTagged _ (DKNewPatient _) = \_ _ -> GT
  compareTagged (DKInSystem k1) (DKInSystem k2) = compareTagged k1 k2

instance Show1 f => ShowTag DetailsKey f where
  showTaggedPrec (DKNewPatient k) = showTaggedPrec k
  showTaggedPrec (DKInSystem k) = showTaggedPrec k

newtype Details f = Details { getDetails :: DMap DetailsKey f }
  deriving (Eq, Ord, Show)

testDMap :: GCompare k
         => DMap k Proxy
         -> DMap k f
         -> DMap k Proxy
testDMap =
  DMap.intersectionWithKey (\_ p _ -> p)

matchesDMap :: (EqTag k Proxy, GCompare k)
            => DMap k Proxy
            -> DMap k f
            -> Bool
matchesDMap dmp dmf =
  dmp == testDMap dmp dmf

injectNewPatient ::
     DMap NewPatientKey f
  -> DMap DetailsKey f
injectNewPatient =
  DMap.mapKeysMonotonic DKNewPatient

newPatientTest :: DMap DetailsKey Proxy
newPatientTest =
  injectNewPatient . DMap.fromList $ [
    NPKInitials :=> Proxy
  , NPKDOB :=> Proxy
  ]

isNewPatient :: DMap DetailsKey f -> Bool
isNewPatient =
  matchesDMap newPatientTest

inSystemTest :: DMap DetailsKey Proxy
inSystemTest =
  DMap.singleton (DKInSystem ISKId) Proxy

isInSystem :: DMap DetailsKey f -> Bool
isInSystem = matchesDMap inSystemTest

class HasDetails k where
  _Details :: forall x. Prism' (k x) (DetailsKey x)

  _DNewPatient :: Prism' (k a) (NewPatientKey a)
  _DNewPatient =
    let
      _DKNewPatient :: Prism' (DetailsKey a) (NewPatientKey a)
      _DKNewPatient = prism DKNewPatient $ \x -> case x of
        DKNewPatient k -> Right k
        _ -> Left x
    in
      _Details . _DKNewPatient

  _DInSystem :: Prism' (k a) (InSystemKey a)
  _DInSystem =
    let
      _DKInSystem :: Prism' (DetailsKey a) (InSystemKey a)
      _DKInSystem = prism DKInSystem $ \x -> case x of
        DKInSystem k -> Right k
        _ -> Left x
    in
      _Details . _DKInSystem

instance HasDetails DetailsKey where
  _Details = id

class HasNewPatient k where
  _NewPatient :: forall x. Prism' (k x) (NewPatientKey x)

  _NPInitials :: Prism' (k Text) ()
  _NPInitials =
    let
      _NPKInitials :: Prism' (NewPatientKey Text) ()
      _NPKInitials = prism (const NPKInitials) $ \x -> case x of
        NPKInitials -> Right ()
    in
      _NewPatient . _NPKInitials

  _NPDOB :: Prism' (k Day) ()
  _NPDOB =
    let
      _NPKDOB :: Prism' (NewPatientKey Day) ()
      _NPKDOB = prism (const NPKDOB) $ \x -> case x of
        NPKDOB -> Right ()
    in
      _NewPatient . _NPKDOB

instance HasNewPatient NewPatientKey where
  _NewPatient = id

instance HasNewPatient DetailsKey where
  _NewPatient = _DNewPatient

class HasInitials k where
  _Initials2 :: Prism' (k Text) ()

instance HasInitials NewPatientKey where
  _Initials2 = _NPInitials

instance HasInitials DetailsKey where
  _Initials2 = _DNewPatient . _NPInitials

newPatientTest2 :: (GCompare k, HasNewPatient k) => DMap k Proxy
newPatientTest2 =
  DMap.fromList [
    _NPInitials # () :=> Proxy
  , _NPDOB      # () :=> Proxy
  ]

isNewPatient2 :: (EqTag k Proxy, GCompare k, HasNewPatient k)
              => DMap k f -> Bool
isNewPatient2 =
  matchesDMap newPatientTest2
