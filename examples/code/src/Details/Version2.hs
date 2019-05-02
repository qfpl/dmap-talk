{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Details.Version2 where

import Control.Monad (join)

import Data.Functor.Classes
import Data.Functor.Identity

import Data.Proxy (Proxy(..))

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Time.Calendar (Day)

import Data.Validation

import Details.Common

data Details f =
  Details {
    dId :: f Id
  , dInitials :: f Text
  , dDOB :: f Day
  , dWeight :: f (Maybe Weight)
  }

deriving instance (Eq (f Id), Eq (f Text), Eq (f Day), Eq (f (Maybe Weight))) => Eq (Details f)
deriving instance (Ord (f Id), Ord (f Text), Ord (f Day), Ord (f (Maybe Weight))) => Ord (Details f)
deriving instance (Show (f Id), Show (f Text), Show (f Day), Show (f (Maybe Weight))) => Show (Details f)

mapDetails :: (forall x. g x -> h x) -> Details g -> Details h
mapDetails f (Details ident initials dob weight) =
  Details (f ident) (f initials) (f dob) (f weight)

traverseDetails :: Applicative f => (forall x. g x -> f (h x)) -> Details g -> f (Details h)
traverseDetails f (Details ident initials dob weight) =
  Details <$> f ident <*> f initials <*> f dob <*> f weight

-- checkFilled :: Details Maybe -> Validation (NonEmptyList DetailsError) (Details Identity)
checkFilled :: Details Maybe -> Maybe (Details Identity)
checkFilled = traverseDetails (fmap Identity)

editFilled :: Details Identity -> Details Maybe
editFilled = mapDetails (Just . runIdentity)

data DetailsError = NotSet | InitialsTooShort
  deriving (Eq, Ord, Show)

validateIdent :: Maybe Id -> Validation [DetailsError] (Identity Id)
validateIdent Nothing = Failure [NotSet]
validateIdent (Just i) = Success (Identity i)

validateInitials :: Maybe Text -> Validation [DetailsError] (Identity Text)
validateInitials Nothing = Failure [NotSet]
validateInitials (Just t)
  | Text.length t < 2 = Failure [InitialsTooShort]
  | otherwise = Success (Identity t)

validateDOB :: Maybe Day -> Validation [DetailsError] (Identity Day)
validateDOB Nothing = Failure [NotSet]
validateDOB (Just d) = Success (Identity d)

validateWeight :: Maybe (Maybe Weight) -> Validation [DetailsError] (Identity (Maybe Weight))
validateWeight mmw = Success (Identity (join mmw))

validateDetails :: Details Maybe -> Validation [DetailsError] (Details Identity)
validateDetails (Details ident initials dob weight) =
  Details <$>
    validateIdent ident <*>
    validateInitials initials <*>
    validateDOB dob <*>
    validateWeight weight

newtype Fn f g h x = Fn { runFn :: g x -> f (h x) }

apDetails :: Applicative f => Details (Fn f g h) -> Details g -> f (Details h)
apDetails (Details fnIdent fnInitials fnDOB fnWeight) (Details ident initials dob weight) =
  Details <$>
    runFn fnIdent ident <*>
    runFn fnInitials initials <*>
    runFn fnDOB dob <*>
    runFn fnWeight weight

detailsFn :: Details (Fn (Validation [DetailsError]) Maybe Identity)
detailsFn =
  Details
  (Fn validateIdent)
  (Fn validateInitials)
  (Fn validateDOB)
  (Fn validateWeight)

validateDetails2 :: Details Maybe -> Validation [DetailsError] (Details Identity)
validateDetails2 =
  apDetails detailsFn

-- instance Eq1 f => Eq (Details f) where
--   Details id1 in1 d1 == Details id2 in2 d2 =
--     eq1 id1 id2 && eq1 in1 in2 && eq1 d1 d2

-- instance Ord1 f => Ord (Details f) where
--   compare (Details id1 in1 d1) (Details id2 in2 d2) =
--     case compare1 id1 id2 of
--       LT -> LT
--       GT -> GT
--       EQ -> case compare1 in1 in2 of
--         LT -> LT
--         GT -> GT
--         EQ -> compare1 d1 d2
