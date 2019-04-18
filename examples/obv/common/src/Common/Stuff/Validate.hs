{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Common.Stuff.Validate where

import Data.Functor.Const
import Data.Functor.Identity
import Data.Bifunctor

import Data.Dependent.Sum (DSum(..))
import Data.Dependent.Map (DMap, GCompare)
import qualified Data.Dependent.Map as DMap

import Data.Validation

import Data.Vessel

import Common.Stuff

newtype Validator e a = Validator { getValidator :: a Maybe -> Validation (a (Const [e])) (a Identity) }

validateDMap :: GCompare k
             => (forall a. k a -> Wrap a Maybe -> Validation (Wrap a (Const [e])) (Wrap a Identity))
             -> Validator e (DMap k)
validateDMap fn =
  Validator $ DMap.traverseWithKey (\k -> bimap (DMap.singleton k . unWrap) unWrap . fn k . Wrap)

wrapValidator :: (Maybe a -> Validation (Const [e] a) (Identity a))
              -> Validator e (Wrap a)
wrapValidator fn =
  Validator $ bimap Wrap Wrap . fn . unWrap

data Error = MissingValue | StrictlyPositiveRequired
  deriving (Eq, Ord, Show)

validateYears :: Validator Error (Wrap Years)
validateYears = wrapValidator $ \m -> case m of
    Just (Years y)
      | y <= 0 -> Failure (Const [StrictlyPositiveRequired])
      | otherwise -> Success (Identity (Years y))
    Nothing -> Failure (Const [MissingValue])

validateBool :: Validator Error (Wrap Bool)
validateBool = wrapValidator $ \m -> case m of
  Just b -> Success (Identity b)
  Nothing -> Success (Identity False)

validateAttendee :: Validator Error Attendee
validateAttendee = Validator $
  let
    f :: AttendeeKey a -> Wrap a Maybe -> Validation (Wrap a (Const [Error])) (Wrap a Identity)
    f k = case k of
      AKProgrammer kp -> case kp of
        PKLanguage _ -> getValidator validateYears
        PKTechnology _ -> getValidator validateBool
      AKManager km -> case km of
        MKRole _ -> getValidator validateYears
        MKMethodology _ -> getValidator validateBool
  in
    bimap Attendee Attendee . getValidator (validateDMap f) . unAttendee

-- data AK2 k where
--   -- we can nest Vessels in here as well :)
--   AK2Manager :: AK2 (DMapV ManagerKey f)
--   AK2Programmer :: AK2 (DMapV ProgrammerKey f)

-- newtype Attendee2 f = Attendee2 { unAttendee2 :: Vessel AK2 f }


-- validation
-- - validation attendee
--   - run through keys, dispatch on manager vs programmer
--     - run through keys, dispatch on key
